import torch
import torch.nn as nn
import torch.nn.functional as F

# Create a Model Class that inherits nn.Module
class Model(nn.Module):
    # Input layer (4 features of the flower) -> 
    # hidden layer1 (number of neurons) ->H2 --> H2(n)

    def __init__(self, in_features, h1=8, h2=9, out_features=2): 
    # number of output features, number of possible output class
    # fc stands for fully connected
        super().__init__()
        self.fc1 = nn.Linear(in_features, h1)
        self.fc2 = nn.Linear(h1, h2)
        self.out = nn.Linear(h2, out_features)

    # this is the function that moves everything forward
    def forward(self, x):
        x = F.relu(self.fc1(x)) # applies relu function
        x = F.relu(self.fc2(x))
        x = self.out(x)

        return x
    

