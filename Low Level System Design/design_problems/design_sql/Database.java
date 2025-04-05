package design_problems.design_sql;

import java.util.HashMap;
import java.util.Map;

public class Database {
    Map<String, Table> tables;

    public Database() {
        this.tables = new HashMap<>();
    }

    public void createTable(String tableName, Map<String, Class<?>> schema) {
        if (tables.containsKey(tableName)) {
            throw new DatabaseException("Table " + tableName + " already exists.");
        }
        this.tables.put(tableName, new Table(tableName, schema));
    }


}
