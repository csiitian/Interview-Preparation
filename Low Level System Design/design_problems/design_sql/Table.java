package design_problems.design_sql;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Table {
    String name;
    List<String> columns;
    List<Map<String, Object>> rows;
    Map<String, Class<?>> columnTypes;

    Table(String name, Map<String, Class<?>> schema) {
        this.name = name;
        this.columns = new ArrayList<>(schema.keySet());
        this.columnTypes = new HashMap<>(schema);
        this.rows = new ArrayList<>();
    }
}
