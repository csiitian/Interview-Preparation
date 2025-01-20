package design_problems.parking_lot;

import design_problems.parking_lot.vehicle.Vehicle;

import java.time.LocalDateTime;

public class ParkingTicket {
    private String ticketNumber;
    private Vehicle vehicle;
    private ParkingSpot parkingSpot;
    private final LocalDateTime issueTime = LocalDateTime.now();
    private Double chargePerHour;

    public String getTicketNumber() {
        return ticketNumber;
    }

    public void setTicketNumber(String ticketNumber) {
        this.ticketNumber = ticketNumber;
    }

    public Vehicle getVehicle() {
        return vehicle;
    }

    public void setVehicle(Vehicle vehicle) {
        this.vehicle = vehicle;
    }

    public ParkingSpot getParkingSpot() {
        return parkingSpot;
    }

    public void setParkingSpot(ParkingSpot parkingSpot) {
        this.parkingSpot = parkingSpot;
    }

    public LocalDateTime getIssueTime() {
        return issueTime;
    }

    public Double getChargePerHour() {
        return chargePerHour;
    }

    public void setChargePerHour(Double chargePerHour) {
        this.chargePerHour = chargePerHour;
    }

    @Override
    public String toString() {
        return "ParkingTicket{" +
                "ticketNumber='" + ticketNumber + '\'' +
                ", vehicle=" + vehicle +
                ", parkingSpot=" + parkingSpot +
                ", issueTime=" + issueTime +
                ", chargePerHour=" + chargePerHour +
                '}';
    }
}
