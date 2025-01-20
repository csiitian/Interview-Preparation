package design_problems.movie_ticket_booking_system.domain;

public class Seat {
    String code;
    SeatType type;
    double price;
    SeatStatus status;

    public Seat(String code, SeatType type, double price, SeatStatus status) {
        this.code = code;
        this.type = type;
        this.price = price;
        this.status = status;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public SeatType getType() {
        return type;
    }

    public void setType(SeatType type) {
        this.type = type;
    }

    public double getPrice() {
        return price;
    }

    public void setPrice(double price) {
        this.price = price;
    }

    public SeatStatus getStatus() {
        return status;
    }

    public void setStatus(SeatStatus status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "Seat{" +
                "code='" + code + '\'' +
                ", type=" + type +
                ", price=" + price +
                ", status=" + status +
                '}';
    }

    public boolean isAvailable() {
        return status == SeatStatus.AVAILABLE;
    }
}
