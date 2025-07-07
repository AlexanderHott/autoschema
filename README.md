# `autoschema`

Automatically find the schema for arbitrary JSON, and generate code based on it.

> [!NOTE]
> This is not battle tested software, and probably will crash if you give it
> really cursed JSON.
>
> The order that the classes are output is also not in the order that they are
> in the hierarchy. PRs welcome!

It can turn something like

```json
[
  {
    "id": 1,
    "name": "Alice",
    "email": "alice@example.com",
    "age": 30
  },
  {
    "user_id": "2b",
    "full_name": "Bob Smith",
    "contact": {
      "email": "bob@example.com",
      "phone": "123-456-7890"
    }
  },
  {
    "id": 3,
    "name": "Charlie",
    "active": true,
    "signup_date": "2023-01-15"
  },
  {
    "name": null,
    "email": "unknown@example.com",
    "metadata": {
      "notes": ["missing id", "name is null"],
      "verified": false
    }
  }
]
```

into

```py
class Contact(msgspec.Struct, frozen=True):
    email: str
    phone: str


class Root(msgspec.Struct, frozen=True):
    active: bool | None = None
    age: int | None = None
    contact: Contact | None = None
    email: str | None = None
    full_name: str | None = None
    id: int | None = None
    metadata: Metadata | None = None
    name: str | None = None
    signup_date: str | None = None
    user_id: str | None = None


class Metadata(msgspec.Struct, frozen=True):
    notes: list[str]
    verified: bool
```

```bash
cargo r -- test.json
```
