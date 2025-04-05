package design_problems.music_recommender.strategy;

import design_problems.music_recommender.Song;
import design_problems.music_recommender.User;

import java.util.List;

public interface MusicRecommendationStrategy {
    List<Song> getRecommendedSongs(User user, List<Song> songs);
}
