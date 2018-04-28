
fn main() {
  for _i in 1..1000000 {
    let mut _v = Vec::new();
    _v.push(Box::new(1));
    _v.push(Box::new(2));
    _v.push(Box::new(3));
    _v.push(Box::new(4));
    _v.push(Box::new(5));
  }
}
