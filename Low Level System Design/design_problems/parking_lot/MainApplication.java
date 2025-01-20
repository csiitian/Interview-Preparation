package design_problems.parking_lot;

import design_problems.parking_lot.vehicle.Car;
import design_problems.parking_lot.vehicle.Truck;
import design_problems.parking_lot.vehicle.Vehicle;

import java.util.ArrayList;
import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        List<ParkingSpot> parkingSpotList = new ArrayList<>();
        parkingSpotList.add(new ParkingSpot(ParkingSlotType.SMALL, null, true, 1, 1, 1));
        parkingSpotList.add(new ParkingSpot(ParkingSlotType.MEDIUM, null, true, 1, 1, 2));
        parkingSpotList.add(new ParkingSpot(ParkingSlotType.LARGE, null, true, 1, 1, 3));
        parkingSpotList.add(new ParkingSpot(ParkingSlotType.MEDIUM, null, true, 1, 1, 4));
        ParkingLot parkingLot = new ParkingLot(parkingSpotList);

        Vehicle car = new Car("ABC1234");
        ParkingTicket carTicket = parkingLot.parkVehicle(car);

        Vehicle truck = new Truck("XYZ1234");
        ParkingTicket truckTicket = parkingLot.parkVehicle(truck);

        parkingLot.printParkedVehicles();

        PaymentReceipt paymentReceipt = parkingLot.removeVehicle(truckTicket);
        System.out.println(paymentReceipt);
    }
}
