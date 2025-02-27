void callee(){
  return x;
}

int main(){
  // 0. only supported by some extension https://stackoverflow.com/q/76046826/21294350
  // 0.a. > the pair of address taken by caller
  // TODO may mean remove the callee addr, so no pair.
  // 0.b. > allow one function to call another, including recursion
  // inling optimization https://cboard.cprogramming.com/c-programming/166765-accessing-stack-caller-post1230694.html?s=4a3143ad7752d22d61b3ad5dbed4e5a2#post1230694
  void callee(){
    return x;
  }
}