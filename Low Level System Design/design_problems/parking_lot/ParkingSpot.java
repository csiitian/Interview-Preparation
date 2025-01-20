package design_problems.parking_lot;

import design_problems.parking_lot.vehicle.Vehicle;

public class ParkingSpot {
    private ParkingSlotType type;
    private Vehicle parkedVehicle;
    private boolean isAvailable;
    private int level;
    private int row;
    private int spotNumber;

    public ParkingSpot(ParkingSlotType type, Vehicle parkedVehicle, boolean isAvailable, int level, int row, int spotNumber) {
        this.type = type;
        this.parkedVehicle = parkedVehicle;
        this.isAvailable = isAvailable;
        this.level = level;
        this.row = row;
        this.spotNumber = spotNumber;
    }

    public ParkingSlotType getType() {
        return type;
    }

    public void setType(ParkingSlotType type) {
        this.type = type;
    }

    public Vehicle getParkedVehicle() {
        return parkedVehicle;
    }

    public void setParkedVehicle(Vehicle parkedVehicle) {
        this.parkedVehicle = parkedVehicle;
    }

    public boolean isAvailable() {
        return isAvailable;
    }

    public void setAvailable(boolean available) {
        isAvailable = available;
    }

    public int getLevel() {
        return level;
    }

    public void setLevel(int level) {
        this.level = level;
    }

    public int getRow() {
        return row;
    }

    public void setRow(int row) {
        this.row = row;
    }

    public int getSpotNumber() {
        return spotNumber;
    }

    public void setSpotNumber(int spotNumber) {
        this.spotNumber = spotNumber;
    }

    @Override
    public String toString() {
        return "ParkingSpot{" +
                "type=" + type +
                ", parkedVehicle=" + parkedVehicle +
                ", isAvailable=" + isAvailable +
                ", level=" + level +
                ", row=" + row +
                ", spotNumber=" + spotNumber +
                '}';
    }
}
