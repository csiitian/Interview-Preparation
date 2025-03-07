package design_patterns.structural.proxy;

import design_patterns.structural.proxy.exception.AuthorizationException;

public class DBReaderProxy implements IDBReaderProxy {
    IDBReader idbReader;

    public DBReaderProxy(IDBReader idbReader) {
        this.idbReader = idbReader;
    }

    @Override
    public int insert(User user, String query) {
        if (!user.getAccessLevelList().contains(AccessLevel.WRITE)) {
            throw new AuthorizationException("User is not authorized to perform write operation.");
        }
        return idbReader.insert(query);
    }

    @Override
    public String select(User user, String query) {
        if (!user.getAccessLevelList().contains(AccessLevel.READ)) {
            throw new AuthorizationException("User is not authorized to perform read operation.");
        }
        return idbReader.select(query);
    }

    @Override
    public int update(User user, String query) {
        if (!user.getAccessLevelList().contains(AccessLevel.WRITE)) {
            throw new AuthorizationException("User is not authorized to perform write operation.");
        }
        return idbReader.update(query);
    }

    @Override
    public int delete(User user, String query) {
        if (!user.getAccessLevelList().contains(AccessLevel.WRITE)) {
            throw new AuthorizationException("User is not authorized to perform write operation.");
        }
        return idbReader.delete(query);
    }
}
