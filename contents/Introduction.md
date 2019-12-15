# Introduction

Neural networks and deep learning are big topics in Computer Science and in the technology industry, they currently provide the best solutions to many problems in image recognition, speech recognition and natural language processing. Recently many papers have been published featuring AI that can learn to paint, build 3D Models, create user interfaces(pix2code), some create images given a sentence and there are many more incredible things being done everyday using neural networks.

I’m writing this series of posts about Neural Networks and Deep learning, where I’m going to guide you from learning the basic concepts of Artificial Neural Networks (ANN), show you examples from simple Network to mimic the AND gate, to solving Image recognition tasks using Convolutional Neural Networks (CNNs), Recurrent Neural Networks (RNN) and more. The code will always be written in python, some times with the help of Tensorflow (I don’t expect you to be guru using Tensorflow as I will try to explain the code in details).

# **Agenda**

- **Introduction To Neural Networks (This post)**
- *AND Gate Neural Network (Perceptron) and XOR Gate Feedfoward Neural Network (2 layers).*
- *Mnist Digit Recognition with CNN*
- *Mnist Digit Recognition with RNN*

# Neural Networks

The definition of a neural network, more properly referred to as an 'artificial' neural network (ANN), is provided by the inventor of one of the first neurocomputers, Dr. Robert Hecht-Nielsen. He defines a neural network as:

> "...a computing system made up of a number of simple, highly interconnected processing elements, which process information by their dynamic state response to external inputs."

Or you can also think of Artificial Neural Network as computational model that is inspired by the way biological neural networks in the human brain process information.

# Biological motivation and connections

The basic computational unit of the brain is a **neuron**. Approximately 86 billion neurons can be found in the human nervous system and they are connected with approximately 10¹⁴ — 10¹⁵ **synapses**. The diagram below shows a cartoon drawing of a biological neuron (left) and a common mathematical model (right).

![img](https://miro.medium.com/max/1251/1*Mz0a4EEsdJYsbvf5M_u-Sw.png)

![img](https://miro.medium.com/max/1087/1*Yf6BWJq0kdHTumErO99bUQ.jpeg)

biological neuron (left) and a common mathematical model (right)

The basic unit of computation in a neural network is the neuron , often called a node or unit. It receives input from some other nodes, or from an external source and computes an output. Each input has an associated
weight (w), which is assigned on the basis of its relative importance to other inputs. The node applies a function to the weighted sum of its inputs.

The idea is that the synaptic strengths (the weights *w*) are learnable and control the strength of influence and its direction: excitory (positive weight) or inhibitory (negative weight) of one neuron on another. In the basic model, the dendrites carry the signal to the cell body where they all get summed. If the final sum is above a certain threshold, the neuron can *fire*, sending a spike along its axon. In the computational model, we assume that the precise timings of the spikes do not matter, and that only the frequency of the firing communicates information. we model the *firing rate* of the neuron with an **activation function** *(e.x sigmoid function)*, which represents the frequency of the spikes along the axon.

Read the original article by David Fumo [here](https://towardsdatascience.com/a-gentle-introduction-to-neural-networks-series-part-1-2b90b87795bc)