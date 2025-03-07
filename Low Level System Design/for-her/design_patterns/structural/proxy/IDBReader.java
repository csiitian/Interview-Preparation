package design_patterns.structural.proxy;

public interface IDBReader {
    int insert(String query);
    String select(String query);
    int update(String query);
    int delete(String query);
}
