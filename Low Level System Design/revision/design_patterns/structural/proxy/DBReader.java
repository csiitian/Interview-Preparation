package design_patterns.structural.proxy;

public class DBReader implements IDBReader {

    @Override
    public int insert(String query) {
        System.out.println("Date Inserted Successfully.");
        return 1;
    }

    @Override
    public String select(String query) {
        System.out.println("Data fetched Successfully.");
        return "Data fetched Successfully.";
    }

    @Override
    public int update(String query) {
        System.out.println("Data Updated Successfully.");
        return 1;
    }

    @Override
    public int delete(String query) {
        System.out.println("Data Deleted Successfully.");
        return 1;
    }
}
