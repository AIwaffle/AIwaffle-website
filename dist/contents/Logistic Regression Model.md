# Logistic Regression Model

## What can it do?
Logistic Regression Model is used for **binary classification** problems, say, seperating 2 types of points on a 2D plane:  
![pic.jpg](https://i.loli.net/2019/12/10/iT9qK4z3WMXwPNE.jpg)  
As you see, the model draw a line seperating these points.  
We call this **Linear Classification**, which means our Logistic Regression Model is a **Linear Classifier**. 

You can also notice that it is neither possible, nor necessary to be 100% correct. This is also a feature of a Machine Learning algorithm.  

By the way, the line dividing the plane is called **Decision Boundary**, which means the prediction of a point is different depending on its relative position to this line.

So, how to build this **Logistic Regression Model**? We need to specify the model structure first.

## Model Structure
$X$: the input tensor  
$X^{(i)}$: the $i^{th}$ value of input $X$  
$Y, Y^{(i)}$: the ground truth tensor / label tensor  
$A$: the activation value tensor  
$A^{[l]}$: the activation value of $l^{th}$ layer
$W^{[l]}$: the `weight` tensor of $l^{th}$ layer


## Linearity and Nonlinearity
If you've read some relevant material on Neuron Network, you will know that in every layer, we do

$$A^{[l]} = \begin{cases}
    W^{[l-1]}X + b^{[l-1]}, \quad l=1\\
    W^{[l-1]}A^{[l-1]} + b^{[l-1]}, \quad l>1
\end{cases} $$

However, the matrix multiplication operation is a *closure operation*, the final output A[l] will still be a linear combination of $X^1...X^n$. This is terrible when we want to solve more than linear problems, so we must introduce some **Nonlinearity**.


## Sigmoid function
![image.png](https://i.loli.net/2019/12/10/qBvW4SY8EJhuCIH.png)

$$Sigmoid(x) = \frac{1}{1 + e^{-x}}$$

If we insert one layer of sigmoid function into our network, the output is no longer linear, the *capacity* of our model increases, which means we can solve more complex problems.

PS: *Sigmoid()* can also map a real value to a probability between 0 and 1.

## Try out the model yourself!
(there is supposed to be a interactive model at the side)