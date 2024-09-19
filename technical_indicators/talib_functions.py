import pandas as pd
import talib


def get_moving_average_indicator(df):
    """
        DEMA - Double Exponential Moving Average - Some NaN values
        EMA - Exponential Moving Average
        KAMA - Kaufman Adaptive Moving Average
        SMA - Simple Moving Average
        TEMA - Triple Exponential Moving Average - Lots of NaN values for some reason (check 20 days)
        TRIMA - Triangular Moving Average - Lots of NaN values
        T3 - Triple Moving Average - Lots of NaN values
        WMA - Weighted Moving Average
        MAMA - MESA Adaptive Moving Average
        MAVP - Moving average with variable period

    Returns:
        _type_: _description_
    """
    close = df['Close']
    list_timeperiod = [5, 10, 20, 50]#, 200]
    for time_p in list_timeperiod:
        df["ma_EMA_"+str(time_p)] = talib.EMA(close, timeperiod=time_p)
        df["ma_KAMA_"+str(time_p)] = talib.KAMA(close, timeperiod=time_p)
        df["ma_SMA_"+str(time_p)] = talib.SMA(close, timeperiod=time_p)
        df["ma_WMA_"+str(time_p)] = talib.WMA(close, timeperiod=time_p)

    return df