
# record

What is record? record is generic type with fields, record has nearly feature of struct, but that has different design.


exmaple:
```
record User {
  name ^string
  email ^string
}
```

It's looking like a struct, but please think, this record defined "map has .name and .email field." here.  
What this different design for? Answer: **Composition!**

Please think "Data" here, What is "Data"?
Data is information of real, Data is uncertained in sometimes.  

So needs this list feature for Data processing:
- Has some features of Data
- Optional
- Generic

When... go back to struct type here.
```
struct User {
  name ^string
  email ^string
}
```
This struct hasn't some features for data processing,
- Certained Data
- Not Optional
- Not Generic

What is best data structure for data processing? it's **map!**
```
{name: "yukari", email: "yukari@example.com"}
```
This structure has other members, and field is optional, and apply to generics functions.

map is good data structure for presentation of real data, but map has a little of cons.
- Uncertained (can access to unknown garbage member)
- Performance (fields is uncertained, so do fields access by runtime)

Solver: **record**
```
record user {
  name ^string
  email ^string
}

record dbmeta {
  id ^int
  saved ^bool
}

instance dbuser = [User DBMeta]

fn new_dbuser(n ^string, e ^string) ^dbuser {
  ^dbuser{name: n, email: e}
}

fn is_correct(u ^user) ^bool {
  u.has_name && u.has_email
}

fn load_from_db(m ^dbmeta) ^inst m {
  db.load(^inst m, m.id)
}
```


