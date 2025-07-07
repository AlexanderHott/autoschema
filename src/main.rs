#![allow(clippy::needless_return)]

use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::Path,
};

use codegen::Class;
use serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum Schema {
    #[default]
    Unknown,
    Null,
    Bool,
    Int,
    Float,
    String,
    List(Box<Schema>),
    Dict(Box<Schema>),
    Struct(Vec<(String, Schema)>),
    Union(Vec<Schema>),
}

impl From<Value> for Schema {
    fn from(value: Value) -> Self {
        match value {
            Value::Null => Schema::Null,
            Value::Bool(_) => Schema::Bool,
            Value::Number(n) => {
                if n.is_f64() {
                    Schema::Float
                } else {
                    Schema::Int
                }
            }
            Value::String(_) => Schema::String,
            Value::Array(arr) => {
                let schemas: Vec<Schema> = arr.into_iter().map(|value| value.into()).collect();
                let schema = Schema::merge_all(schemas);

                Schema::List(Box::new(schema))
            }
            Value::Object(obj) => {
                let any_field_has_at_sign = obj.iter().any(|(k, _)| k.contains('@'));
                let all_fields_numeric =
                    !obj.is_empty() && obj.iter().all(|(k, _)| k.chars().all(char::is_numeric));

                if any_field_has_at_sign || all_fields_numeric {
                    let fields = obj.into_iter().map(|(_, val)| val.into()).collect();
                    Schema::Dict(Box::new(Schema::Union(fields)))
                } else {
                    let fields = obj
                        .into_iter()
                        .map(|(key, val)| (key, val.into()))
                        .collect();
                    Schema::Struct(fields)
                }
            }
        }
    }
}

impl Schema {
    /// Merge all the schemas together.
    pub fn merge_all(schemas: Vec<Schema>) -> Self {
        schemas
            .into_iter()
            .reduce(|acc, schema| acc.merge(schema))
            .unwrap_or_default()
    }

    fn merge_structs_conservative(
        a: Vec<(String, Schema)>,
        b: Vec<(String, Schema)>,
    ) -> Vec<(String, Schema)> {
        let mut a_map: HashMap<String, Schema> = HashMap::from_iter(a);
        let b_map: HashMap<String, Schema> = HashMap::from_iter(b);
        let mut result = HashMap::new();

        for (key, b_type) in b_map {
            if let Some(a_type) = a_map.remove(&key) {
                result.insert(key, a_type.merge(b_type));
            } else {
                result.insert(key, b_type.merge(Schema::Null));
            }
        }

        for (key, a_type) in a_map {
            result.insert(key, a_type.merge(Schema::Null));
        }

        result.into_iter().collect()
    }

    pub fn merge(self, other: Schema) -> Schema {
        if self == other {
            return self;
        }

        match (self, other) {
            (Schema::List(a), Schema::List(b)) => Schema::List(Box::new(a.merge(*b))),

            // Struct unions - merge fields, union overlapping field types, make disjoint types
            // nullable.
            (Schema::Struct(a), Schema::Struct(b)) => {
                Schema::Struct(Self::merge_structs_conservative(a, b))
            }

            // Dicts - merge value types
            (Schema::Dict(a), Schema::Dict(b)) => Schema::Dict(Box::new(a.merge(*b))),

            // Dicts & Structs - dicts "erase" the struct keys, unioning value types
            (Schema::Dict(a), Schema::Struct(b)) => {
                let mut schemas: Vec<Schema> = b.into_iter().map(|(_, v)| v).collect();
                schemas.push(*a);
                Schema::Dict(Box::new(Schema::merge_all(schemas)))
            }
            (Schema::Struct(a), Schema::Dict(b)) => {
                let mut schemas: Vec<Schema> = a.into_iter().map(|(_, v)| v).collect();
                schemas.push(*b);
                Schema::Dict(Box::new(Schema::merge_all(schemas)))
            }

            // Union & Struct - make sure Union contains only one struct
            (Schema::Union(b), Schema::Struct(mut a)) => {
                let first_non_struct = b
                    .iter()
                    .find(|&schema| !matches!(schema, Schema::Struct(_)));

                if let Some(mut base) = first_non_struct.cloned() {
                    for schema in b {
                        if let Schema::Struct(inner) = schema {
                            a = Schema::merge_structs_conservative(a, inner)
                        } else {
                            base = base.merge(schema);
                        }
                    }
                    base.merge(Schema::Struct(a))
                } else {
                    for schema in b {
                        if let Schema::Struct(inner) = schema {
                            a = Schema::merge_structs_conservative(a, inner)
                        } else {
                            unreachable!();
                        }
                    }
                    Schema::Struct(a)
                }
            }
            (Schema::Struct(mut a), Schema::Union(b)) => {
                let first_non_struct = b
                    .iter()
                    .find(|&schema| !matches!(schema, Schema::Struct(_)));

                if let Some(mut base) = first_non_struct.cloned() {
                    for schema in b {
                        if let Schema::Struct(inner) = schema {
                            a = Schema::merge_structs_conservative(a, inner)
                        } else {
                            base = base.merge(schema);
                        }
                    }
                    base.merge(Schema::Struct(a))
                } else {
                    for schema in b {
                        if let Schema::Struct(inner) = schema {
                            a = Schema::merge_structs_conservative(a, inner)
                        } else {
                            unreachable!();
                        }
                    }
                    Schema::Struct(a)
                }
            }

            // Unions - flatten union types
            (Schema::Union(mut a), Schema::Union(b)) => {
                a.extend(b);
                Self::flatten_union(a)
            }
            (Schema::Union(mut a), b) => {
                a.push(b);
                Self::flatten_union(a)
            }
            (a, Schema::Union(mut b)) => {
                b.push(a);
                Self::flatten_union(b)
            }

            // Different types - create union
            (a, b) => Schema::Union(vec![a, b]),
        }
    }

    /// Flatten and deduplicate union types
    fn flatten_union(mut schemas: Vec<Schema>) -> Schema {
        let mut seen = HashSet::new();
        schemas.retain(|schema| seen.insert(schema.clone()));
        Schema::merge_all(schemas)
    }
}

mod codegen {
    use std::fmt::Display;

    use convert_case::{Case, Casing};

    use crate::Schema;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub enum Assignment {
        #[default]
        None,
        NoDefault(String),
        Default(String),
    }

    impl Display for Assignment {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Assignment::None => Ok(()),
                Assignment::NoDefault(s) | Assignment::Default(s) => write!(f, " = {s}"),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Field {
        pub name: String,
        pub ty: String,
        pub assignment: Assignment,
    }

    impl Display for Field {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            writeln!(f, "    {}: {}{}", self.name, self.ty, self.assignment)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Class {
        pub name: String,
        pub meta: Vec<String>,
        pub fields: Vec<Field>,
    }

    impl Display for Class {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            writeln!(f, "class {}({}):", self.name, self.meta.join(", "))?;

            let mut fields = self.fields.clone();
            fields.sort_by(|a, b| (&a.assignment, &a.name).cmp(&(&b.assignment, &b.name)));
            for field in &fields {
                field.fmt(f)?
            }
            Ok(())
        }
    }

    fn type_name(field_name: &String, ty: &Schema) -> String {
        match ty {
            Schema::Unknown => "t.Any".into(),
            Schema::Null => "None".into(),
            Schema::Bool => "bool".into(),
            Schema::Int => "int".into(),
            Schema::Float => "float".into(),
            Schema::String => "str".into(),
            Schema::List(schema) => format!("list[{}]", type_name(field_name, schema.as_ref())),
            Schema::Dict(schema) => {
                format!("dict[str, {}]", type_name(field_name, schema.as_ref()))
            }
            Schema::Struct(_) => field_name.to_case(Case::Pascal),
            Schema::Union(schemas) => {
                let mut i = 1;
                let type_names: Vec<String> = schemas
                    .iter()
                    .map(|schema| match schema {
                        ty2 @ Schema::Unknown
                        | ty2 @ Schema::Null
                        | ty2 @ Schema::Bool
                        | ty2 @ Schema::Int
                        | ty2 @ Schema::Float
                        | ty2 @ Schema::String
                        | ty2 @ Schema::List(_)
                        | ty2 @ Schema::Dict(_) => type_name(field_name, ty2),
                        Schema::Struct(_) => {
                            let name = if i == 1 {
                                field_name.to_case(Case::Pascal)
                            } else {
                                format!("{}{i}", field_name.to_case(Case::Pascal))
                            };
                            i += 1;
                            name
                        }
                        Schema::Union(_) => unreachable!(
                            "Unions cannot contain unions because they would be combined."
                        ),
                    })
                    .collect();
                type_names.join(" | ")
            }
        }
    }

    fn field_assignment(field_name: &String, ty: &Schema) -> Assignment {
        fn has_null(ty: &Schema) -> bool {
            match ty {
                Schema::Unknown
                | Schema::Bool
                | Schema::Int
                | Schema::Float
                | Schema::String
                | Schema::List(_)
                | Schema::Dict(_)
                | Schema::Struct(_) => false,
                Schema::Null => true,
                Schema::Union(schemas) => schemas.iter().any(has_null),
            }
        }

        let needs_default = has_null(ty);
        let needs_rename = field_name.chars().next().is_some_and(char::is_numeric)
            || &sanatize_field_name(field_name) != field_name;
        match (needs_default, needs_rename) {
            (false, false) => Assignment::None,
            (false, true) => {
                Assignment::NoDefault(format!(r#"msgspec.field(name="{field_name}")"#))
            }
            (true, false) => Assignment::Default("None".into()),
            (true, true) => Assignment::Default(format!(
                r#"msgspec.field(name="{field_name}", default=None)"#
            )),
        }
    }

    fn sanatize_field_name(name: &String) -> String {
        let mut name_new = name.to_case(Case::Snake);
        if name.chars().next().is_some_and(char::is_numeric) {
            name_new.insert(0, '_');
        }
        name_new
    }

    impl From<Schema> for Vec<Class> {
        fn from(value: Schema) -> Self {
            if let Schema::Struct(strukt_initial) = value {
                let mut to_process = vec![("Root".to_string(), strukt_initial)];
                let mut classes = Vec::new();

                fn extract_struct(
                    field_name: &String,
                    ty: &Schema,
                ) -> Option<Vec<(String, Vec<(String, Schema)>)>> {
                    match ty {
                        Schema::Unknown
                        | Schema::Null
                        | Schema::Bool
                        | Schema::Int
                        | Schema::Float
                        | Schema::String => None,
                        Schema::Struct(items) => Some(vec![(field_name.clone(), items.clone())]),
                        Schema::List(schema) | Schema::Dict(schema) => {
                            extract_struct(field_name, schema)
                        }
                        Schema::Union(schemas) => {
                            let mut i = 1;
                            let mut field_name_count = field_name.clone();
                            let mut structs = Vec::new();
                            for schema in schemas {
                                if let Some(structs_extracted) =
                                    extract_struct(&field_name_count, schema)
                                {
                                    let first_extracted =
                                        structs_extracted.into_iter().next().unwrap();

                                    structs.push(first_extracted);
                                    i += 1;
                                    field_name_count = format!("{field_name}{i}");
                                }
                            }
                            Some(structs)
                        }
                    }
                    // if let Schema::Struct(ty2) = ty {
                    //     println!("adding {ty2:?}");
                    //     to_process.push((field_name.clone(), ty2.clone()));
                    // } else if let Schema::List(ty2) | Schema::Dict(ty2) = ty {
                    //     println!("adding {ty2:?}");
                    //     to_process.push((field_name.clone(), (*ty2).clone()));
                    // };
                }

                while let Some((name, strukt)) = to_process.pop() {
                    let fields = strukt
                        .iter()
                        .map(|(field_name, ty)| {
                            if let Some(structs) = extract_struct(field_name, ty) {
                                to_process.extend(structs);
                            }

                            Field {
                                name: sanatize_field_name(field_name),
                                ty: type_name(field_name, ty),
                                assignment: field_assignment(field_name, ty),
                            }
                        })
                        .collect();

                    classes.push(Class {
                        name: name.to_case(Case::Pascal),
                        meta: vec!["msgspec.Struct".into(), "frozen=True".into()],
                        fields,
                    });
                }
                classes
            } else if let Schema::List(inner) = value {
                (*inner).into()
            } else {
                panic!("only works for strukts at the top level")
            }
        }
    }
}

type DependencyGraph = HashMap<String, HashSet<String>>;

fn build_dependency_graph(classes: &Vec<codegen::Class>) -> DependencyGraph {
    let available_types: Vec<String> = classes.iter().map(|class| class.name.clone()).collect();
    let mut graph = HashMap::with_capacity(available_types.len());

    for class in classes {
        let mut deps = HashSet::new();
        for field in &class.fields {
            if available_types.contains(&field.ty) {
                deps.insert(field.ty.clone());
            }
        }
        graph.insert(class.name.clone(), deps);
    }

    graph
}

fn topo_sort(graph: &DependencyGraph) -> Vec<String> {
    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut all_nodes: HashSet<String> = HashSet::new();

    for (node, deps) in graph {
        all_nodes.insert(node.clone());
        for dep in deps {
            all_nodes.insert(dep.clone());
        }
    }

    for node in &all_nodes {
        in_degree.insert(node.clone(), 0);
    }

    for deps in graph.values() {
        for dep in deps {
            *in_degree.get_mut(dep).unwrap() += 1;
        }
    }

    let mut queue: VecDeque<String> = VecDeque::new();
    for (node, &degree) in &in_degree {
        if degree == 0 {
            queue.push_back(node.clone());
        }
    }

    let mut result: Vec<String> = Vec::new();

    while let Some(node) = queue.pop_front() {
        result.push(node.clone());

        if let Some(deps) = graph.get(&node) {
            for dep in deps {
                let degree = in_degree.get_mut(dep).unwrap();
                *degree -= 1;
                if *degree == 0 {
                    queue.push_back(dep.clone());
                }
            }
        }
    }

    if result.len() != all_nodes.len() {
        panic!("Warning: Cycle detected in dependency graph");
    }

    result.reverse();
    result
}

fn msgspec_codegen(schema: Schema) -> String {
    let classes: Vec<codegen::Class> = schema.into();
    let graph = build_dependency_graph(&classes);
    let class_order = topo_sort(&graph);
    let mut classes_map: HashMap<String, Class> =
        HashMap::from_iter(classes.into_iter().map(|class| (class.name.clone(), class)));

    let mut ordered_classes = Vec::new();
    for class_key in &class_order {
        ordered_classes.push(classes_map.remove(class_key).unwrap())
    }

    ordered_classes
        .iter()
        .map(|class| class.to_string())
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() < 2 {
        return Err(format!("Usage: {} <path to file>", args[0]).into());
    }
    let filename = &args[1];
    let path = Path::new(filename);
    if !path.exists() {
        return Err(format!("File {filename} doesn't exist.").into());
    }
    if !path.is_file() {
        return Err(format!("{filename} is not a file.").into());
    }

    let json_str = std::fs::read_to_string(path)?;

    let json_value: Value = serde_json::from_str(&json_str)?;
    let schema = Schema::from(json_value);
    let code = msgspec_codegen(schema);
    println!("{code}");

    return Ok(());
}
