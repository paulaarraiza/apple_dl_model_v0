import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker 


def plot_price_ta(df):
    """This function plots prices and MAs for a given stock dataframe. 

    Args:
        df (_type_): _description_
    """
    # Plotting
    plt.figure(figsize=(12, 6))
    plt.plot(df.index, df['Close'], label='Close Price', color='blue')
    plt.plot(df.index, df['ma_EMA_5'], label='EMA 5', color='orange', linestyle='--')
    plt.plot(df.index, df['ma_EMA_20'], label='EMA 20', color='green', linestyle='--')
    plt.plot(df.index, df['ma_EMA_50'], label='EMA 50', color='red', linestyle='--')

    plt.xlabel('Date')
    plt.ylabel('Price')
    plt.title('Closing Price and Moving Averages Over Time')
    plt.legend()
    plt.grid(True)

    # Improve date formatting
    plt.gca().xaxis.set_major_locator(ticker.MaxNLocator(20))  # Limit number of ticks to avoid clutter
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))

    plt.xticks(rotation=45)
    plt.tight_layout()

    # Show plot
    plt.show()