
# Resource Region

**DRAFT**

Region can be control machine resources, with scope.  
Memory is default control resource by Region, but is it not applicable to other resources?, A. Can do.

I consider GPU Resource control by region at here.

```
fn vulkan_main() {
  in_vulkan[ctx] { # initialize Vulkan, Context to bind as `ctx variable.
    # ... vulkan rendering|compute code.
  } # release Vulkan.
}
vulkan_main()
```
**this case is correct.**

**but this case...**
```
fn vulkan_main() {
  var outctx VulkanCtx # VulkanCtx variable outside of vulkan scope.
  in_vulkan[ctx] {
    outctx = ctx # WTF, this is illegal code, but raise Segfault at runtime...... (. >_<)
  }
}
vulkan_main()
```

So introduce Region Typing to this case.

```
fn vulkan_main() {
  var outctx VulkanCtx # outctx <- VulkanCtx${HVR}
  in_vulkan[ctx] { # Vulkan Region, ctx <- VulkanCtx${VR1}
    outctx = ctx # illegal at compile time: not outctx${VHR} <: ctx${VR1}
  } # Release Vulkan
}
vulkan_main()
```

