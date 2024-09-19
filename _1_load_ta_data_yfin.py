"""
0. Load the data
"""

import yfinance as yf
import pandas as pd
from technical_indicators import talib_functions

def get_ta_historical_dataset(ticker_symbol, timewindow):

    # Get the data for the stock
    stock_data = yf.Ticker(ticker_symbol)

    # Fetch historical stock data (e.g., last 5 years of data)
    historical_data = stock_data.history(period=timewindow)
    historical_data = historical_data.iloc[:, :-1] # remove stock splits column

    ta_data = talib_functions.get_moving_average_indicator(historical_data)

    return ta_data
