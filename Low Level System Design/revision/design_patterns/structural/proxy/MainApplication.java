package design_patterns.structural.proxy;

import design_patterns.structural.proxy.exception.AuthorizationException;

import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        IDBReader idbReader = new DBReader();
        IDBReaderProxy idbReaderProxy = new DBReaderProxy(idbReader);
        User readUser = new User("1", "Vishal", List.of(AccessLevel.READ));
        try {
            idbReaderProxy.insert(readUser, "Insert into users (name) values (Vishal)");
        } catch (AuthorizationException e) {
            System.err.println(e.getMessage());
        }
        try {
            idbReaderProxy.select(readUser, "select * from users");
        } catch (AuthorizationException e) {
            System.err.println(e.getMessage());
        }

        User writeUser = new User("2", "Satish", List.of(AccessLevel.WRITE));
        try {
            idbReaderProxy.insert(writeUser, "Insert into users (name) values (satish)");
        } catch (AuthorizationException e) {
            System.err.println(e.getMessage());
        }
    }
}
