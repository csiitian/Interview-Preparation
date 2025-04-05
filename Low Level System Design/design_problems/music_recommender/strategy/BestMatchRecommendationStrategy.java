package design_problems.music_recommender.strategy;

import design_problems.music_recommender.PlayList;
import design_problems.music_recommender.Singer;
import design_problems.music_recommender.Song;
import design_problems.music_recommender.User;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BestMatchRecommendationStrategy implements MusicRecommendationStrategy {
    @Override
    public List<Song> getRecommendedSongs(User user, List<Song> songs) {
        List<PlayList> userPlalist = user.getPlayLists();
        Map<Singer, Long> singerScore = new HashMap<>();
        Map<String, Long> genreScore = new HashMap<>();
        Map<String, Long> tempoScore = new HashMap<>();
        for (PlayList playList: userPlalist) {
            for (Song song: playList.getSongs()) {
                for (Singer singer: song.getSinger()) {
                    singerScore.put(singer, singerScore.getOrDefault(singer, 0L) + 1);
                }
                genreScore.put(song.getCatalog().getGenre(), genreScore.getOrDefault(song.getCatalog().getGenre(), 0L) + 1);
                tempoScore.put(song.getCatalog().getTempo(), tempoScore.getOrDefault(song.getCatalog().getTempo(), 0L) + 1);
            }
        }

        songs.sort((a, b) -> {
            int aMatchingCount = getMatchingCount(a, singerScore, genreScore, tempoScore);
            int bMatchingCount = getMatchingCount(b, singerScore, genreScore, tempoScore);
            if (aMatchingCount != bMatchingCount) {
                return bMatchingCount - aMatchingCount;
            }
            
            long aGenreScore = genreScore.getOrDefault(a.getCatalog().getGenre(), 0L);
            long bGenreScore = genreScore.getOrDefault(b.getCatalog().getGenre(), 0L);
            if (aGenreScore != bGenreScore) {
                return Math.toIntExact(bGenreScore - aGenreScore);
            }

            long aSingerScore =
                    a.getSinger().stream().map(singer -> singerScore.getOrDefault(singer, 0L)).reduce(Long::sum).orElse(0L);
            long bSingerScore =
                    b.getSinger().stream().map(singer -> singerScore.getOrDefault(singer, 0L)).reduce(Long::sum).orElse(0L);
            if (aSingerScore != bSingerScore) {
                return Math.toIntExact(bSingerScore - aSingerScore);
            }

            long aTempScore = tempoScore.getOrDefault(a.getCatalog().getTempo(), 0L);
            long bTempScore = tempoScore.getOrDefault(b.getCatalog().getTempo(), 0L);
            return Math.toIntExact(bTempScore - aTempScore);
        });

        return songs;
    }

    private int getMatchingCount(Song song, Map<Singer, Long> singerScore, Map<String, Long> genreScore, Map<String,
            Long> tempoScore) {
        return (song.getSinger().stream().anyMatch(singerScore::containsKey) ? 1 : 0) +
                (genreScore.containsKey(song.getCatalog().getGenre()) ? 1 : 0) +
                (tempoScore.containsKey(song.getCatalog().getTempo()) ? 1 : 0);
    }
}
