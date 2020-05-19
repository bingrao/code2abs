public static void removeStock(android.content.Context context, java.lang.String symbol) {
    com.udacity.stockhawk.data.PrefUtils.editStockPref(context, symbol, false);
}