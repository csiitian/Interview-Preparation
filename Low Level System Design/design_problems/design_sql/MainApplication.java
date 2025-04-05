package design_problems.design_sql;

import java.util.HashMap;
import java.util.Map;

public class MainApplication {
    public static void main(String[] args) {
        Database database = new Database();
        Map<String, Class<?>> schema = new HashMap<>();
        schema.put("column1", Integer.class);
        schema.put("column2", String.class);
        database.createTable("example", schema);
    }
}
