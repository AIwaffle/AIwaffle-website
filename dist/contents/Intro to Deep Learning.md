# Intro to Deep Learning

## What is deep learning?
Deep learning is a subfield of ML, which use neural networks to learn features from complex data.

For example, the task of planear classification is to separate two types of points on a 2D plane.

## From Planear Classification to Deep Learning
![image.png](https://i.loli.net/2020/01/12/gemtvVBkQzAsSfn.png)

This is a fairly easy problem even without using ML techniques. A plausible solution is to try out every line (this is called `decision boundary`)  by iterating $k$ and $b$ in $y = kx+b$, and selecting the $k$ and $b$ with the highest accuracy.
We may write these codes:
```python
...
if k * point.x + b > point.y:
	# I think the point is red!
else:
	# I think the point is blue!
...
```

In the graph below, the green line has an accuracy of $\frac{2}{33}$, and the highest accuracy we can ever achieve with a linear decision boundary is $\frac{1}{33}$.
![image.png](https://i.loli.net/2020/01/12/Ffx35pMTHsG4tOK.png)

However, the former method can not solve this situation (seemingly): 
![image.png](https://i.loli.net/2020/01/12/IH6mUq1ziBC2JbQ.png)
There is still ways to do it.
If we add an z axis to represent $|x - y|$...
![image.png](https://i.loli.net/2020/01/12/e5JY2QkRhgrXvGp.png)
The problem becomes a three-dimensional. Remember, the goal is to predict the `color` of the points given their $x$ and $y$. It doesn't matter if we reshape the problem to 3D.

Then, we got our solution:
We loop over $a, b, c$ in the expression of a plane: $ax + by + cz = 1$, then using the best plane as our decision boundary.

In this approach, we added a *feature* with 2 existing *features* to our data.

**Feature**: The attribute of an object. *Features* are the inputs of our ML model.
**Feature Cross**: To combine 2 or more features to generate new feature.

This approach is generalizable to higher dimensions by doing more feature crosses. But it is hard to select the proper feature by hand. Like the graph below.
![image.png](https://i.loli.net/2020/01/12/2WglThiLJnSas8N.png)

So, we introduce *Deep Learning*.

## Neural network
![image.png](https://i.loli.net/2020/01/12/t5lyLQTqNIZuHx1.png)
This is a neuron. A neuron can be understood as a container of a value, called *activation value* ($a$). It takes several inputs $x_i$ and outputs an scalar $y$. It does the following calculation:
$$a = \sum{w_ix_i + b_i}$$
$$y = \text{activation}(a) \text{ or } \sigma(a)$$
The *activation function* is a nonlinear function, such as [ReLU](https://en.wikipedia.org/wiki/Rectifier_(neural_networks)). We will explain this later.

![image.png](https://i.loli.net/2020/01/12/vuzyPFrGVhsjd4I.png)
Above is a neural network example. We suppose you know matrix multiplication, so we can describe the effect of any layer as:
$$Y = \sigma(WX+B)$$
$$W = \begin{bmatrix}
w_{11} &w_{12}  & \cdots &w_{1m} \\ 
w_{21} & \ddots &  & \\ 
\vdots &  & \ddots & \\ 
w_{n1} &  &  & w_{nm}
\end{bmatrix}, B = \begin{bmatrix}
b_1\\ 
b_2\\ 
\vdots \\ 
b_n
\end{bmatrix}$$
$$m\text{: the size of previous layer}, n\text{: size of current layer}$$
Try to calculate the shape of $X$ and $Y$ by yourself!

## Activation function
Imagine what will happen without *activation function*. The output of each layer will be a linear combination of the inputs! But we know that sometimes a linear solution is not the key, recall the spiral graph.
If we insert nonlinear functions like [ReLU](https://en.wikipedia.org/wiki/Rectifier_(neural_networks)) or [sigmoid](https://en.wikipedia.org/wiki/Sigmoid_function), the output becomes complex. Moreover, there are mathematical proofs that we can approximate any functions with lots of *neurons* and *activation functions*.

## Reminder
We are going to skip the part of *gradient descent* since it is hard to demonstrate with text and images.
You should prepare the knowledge of *gradient descent* before our next course.

Useful resources:
**Gradient descent by 3b1b**
[Youtube](https://www.youtube.com/watch?v=IHZwWFHWa-w&list=PLZHQObOWTQDNU6R1_67000Dx_ZCJB-3pi&index=2) [bilibili](https://www.bilibili.com/video/av16144388)

---
##### Author: [Yulun Wu](https://github.com/IDl0T)

*[ML]: Machine Learning
*[DL]: Deep Learning