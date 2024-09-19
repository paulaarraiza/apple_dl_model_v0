import torch
import torch.nn as nn
import torch.nn.functional as F
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker  # Correct import for MaxNLocator
from _1_load_ta_data_yfin import get_ta_historical_dataset
from _2_build_model import Model
from plotting_functions.plot_prices_mas import plot_price_ta
from preprocessing_functions import preprocess_torch

ticker_symbol = 'AAPL'
timewindow = "2y"

print("Executing")
df = get_ta_historical_dataset(ticker_symbol, timewindow)
print("Df generated")

# Create a new column, Target, with Close lagged by one period
df['Target'] = df['Close'].shift(1)

# Drop NA values since they are first observtions
df = df.dropna()

# Generate class: growth in one day or not
df['Growth'] = df.apply(lambda row: 1 if row['Target'] > row['Close'] else 0, axis=1)

# Convert 'Date' column to datetime format
df.index = pd.to_datetime(df.index)

# Plot prices and technical indicators
plot_price_ta(df)

# Separate features from target
X = df.drop(['Open', 'High', 'Low', 'Target'], axis=1)
y = df['Growth']

X_train, X_test, y_train, y_test, num_features = preprocess_torch(X, y)

# Initialize model
torch.manual_seed(9)
model = Model(in_features=num_features)

# Set criterion to measure the error
criterion = nn.CrossEntropyLoss()

# Choose Adam Optimizer, theres a lot, lr = learning_rate
# if error does not go down, then decrease lr. The lower, the slower the program
optimizer = torch.optim.Adam(model.parameters(), lr=0.1)

# Train our model
# Epochs? (one run through all the training data)
epochs =200
losses = []
for i in range(epochs):
    # Go forward and get a prediction
    y_pred = model.forward(X_train)

    # Measure the loss
    loss = criterion(y_pred, y_train) # predicted vs forecast values

    # Keep track of our losses
    losses.append(loss.detach().numpy())

    # Print every 10 epoch
    if i % 10 ==0:
        print(f"Epoch: {i} and loss: {loss}")

    # Do some back propagation: take the error rate of forward propagation and feed it back
    # through the network to fine tune the weights

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
    
# Graph it out
plt.plot(range(epochs), losses)
plt.ylabel("loss/error")
plt.xlabel("Epoch")