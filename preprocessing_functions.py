import torch
from sklearn.model_selection import train_test_split
import pandas as pd

def preprocess_columns_and_nas(df):
    """This function generates target column and preprocesses date

    Args:
        df (_type_): _description_
    """

    # Create a new column, Target, with Close lagged by one period
    df['Target'] = df['Close'].shift(1)

    # Drop NA values since they are first observtions
    df = df.dropna()

    # Generate class: growth in one day or not
    df['Growth'] = df.apply(lambda row: 1 if row['Target'] > row['Close'] else 0, axis=1)

    # Convert 'Date' column to datetime format
    df.index = pd.to_datetime(df.index)
    
    return df


def preprocess_torch(X, y):
    """This function preprocesses a df with Y column and X columns and converts to torch data types

    Args:
        df (_type_): _description_
    """
    
    # Convert to numpy array
    X = X.values
    y = y.values

    # Train test split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size =0.2, random_state =7)

    # Convert to pytorch appropiate data types
    X_train = torch.FloatTensor(X_train) # float because features are float
    X_test = torch.FloatTensor(X_test) 
    y_train = torch.LongTensor(y_train) # 64 bip integers
    y_test = torch.LongTensor(y_test) 

    num_features = X_train.shape[1]
    
    return X_train, X_test, y_train, y_test, num_features