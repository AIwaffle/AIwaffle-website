# Types of Neural Networks

There are many classes of neural networks and these classes also have sub-classes, here I will list the most used ones and make things simple to move on in this journey to learn neural networks.

## 1. Feedforward Neural Network

A feedforward neural network is an artificial neural network where connections between the units do *not* form a cycle. In this network, the information moves in only one direction, forward, from the input nodes, through the hidden nodes (if any) and to the output nodes. There are no cycles or loops in the network.

We can distinguish two types of feedforward neural networks:

## **1.1. Single-layer Perceptron**

This is the simplest feedforward neural Network and does not contain any hidden layer, Which means it only consists of a single layer of output nodes. This is said to be single because when we count the layers we do not include the input layer, the reason for that is because at the input layer no computations is done, the inputs are fed directly to the outputs via a series of weights.

![img](https://miro.medium.com/max/515/1*1WVSZV59q750l6ERCcRgCg.gif)

Simple Perceptron

## 1.2. Multi-layer perceptron (MLP)

This class of networks consists of multiple layers of computational units, usually interconnected in a feed-forward way. Each neuron in one layer has directed connections to the neurons of the subsequent layer. In many applications the units of these networks apply a sigmoid function as an activation function. MLP are very more useful and one good reason is that, they are able to learn non-linear representations (most of the cases the data presented to us is not linearly separable), we will come back to analyse this point in the example I’ll show you in the next post.

![img](https://miro.medium.com/max/1056/1*-NE4oK759a3uqKzI6McJwA.jpeg)

MLP

## 1.3. Convolutional Neural Network (CNN)

Convolutional Neural Networks are very similar to ordinary Neural Networks, they are made up of neurons that have learnable weights and biases. In convolutional neural network (CNN, or ConvNet or shift invariant or space invariant) the unit connectivity pattern is inspired by the organization of the visual cortex, Units respond to stimuli in a restricted region of space known as the receptive field. Receptive fields partially overlap, over-covering the entire visual field. Unit response can be approximated mathematically by a convolution operation. They are variations of multilayer perceptrons that use minimal preprocessing. Their wide applications is in image and video recognition, recommender systems and natural language processing. CNNs requires large data to train on.

![img](https://miro.medium.com/max/2571/1*N4h1SgwbWNmtrRhszM9EJg.png)

CNN for image classification

## 2. Recurrent neural networks

In recurrent neural network (RNN), connections between units form a directed cycle (they propagate data forward, but also backwards, from later processing stages to earlier stages). This allows it to exhibit dynamic temporal behavior. Unlike feedforward neural networks, RNNs can use their internal memory to process arbitrary sequences of inputs. This makes them applicable to tasks such as unsegmented, connected handwriting recognition, speech recognition and other general sequence processors.

Read the original article by David Fumo [here](https://towardsdatascience.com/a-gentle-introduction-to-neural-networks-series-part-1-2b90b87795bc)