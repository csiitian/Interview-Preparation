package design_patterns.structural.proxy;

public interface IDBReaderProxy {
    int insert(User user, String query);
    String select(User user, String query);
    int update(User user, String query);
    int delete(User user, String query);
}
