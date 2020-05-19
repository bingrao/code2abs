@java.lang.Override
public beans.CurrencyDefault selectCurrencyDefault(beans.CurrencyDefault currencyD) throws java.rmi.RemoteException {
    return dbSQL.CurrencyDefaultSQL.selectcurrencyDefault(currencyD.getCurrency_code(), dsSQL.getConn());
}