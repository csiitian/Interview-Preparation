package design_problems.flip_fit;

import design_problems.flip_fit.domain.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FlipFitService {
    List<Center> centers;
    List<SlotReservation> reservations;
    List<User> users;

    FlipFitService() {
        centers = new ArrayList<>();
        reservations = new ArrayList<>();
        users = new ArrayList<>();

        createDummyUsers();
    }

    void createDummyUsers() {
        User user1 = new User("1", "Vishal Singh");
        User user2 = new User("2", "Vikas");
        users.addAll(Arrays.asList(user1, user2));
    }
}
