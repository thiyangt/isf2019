## Test animations

```{r fig.show='animate',out.width="60%", interval=1/10}
ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
```

##

```{r fig.show='animate',out.width="60%", out.height="60%",interval=1/10, dev.args=list(bg="transparent"), controls="none"}

f <- flea[, 1:6]
animate(f, grand_tour(), display_xy())
# or in short
animate(f)
animate(f, max_frames = 30)

```
