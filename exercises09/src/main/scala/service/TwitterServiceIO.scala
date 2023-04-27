package service

import cats.effect.IO
import cats.syntax.all._
import service.domain._
import twitter.TwitterApi
import twitter.domain._

import scala.util.Try

class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {

  def getIOWithAsync[A](f: (Try[A] => Unit) => Unit): IO[A] = IO.async_(callback => f(callback.compose(_.toEither)))

  def tweet(user: User, text: String): IO[TweetId] = getIOWithAsync(api.tweet(user, text))

  def like(user: User, tweetId: TweetId): IO[Unit] =
    getIOWithAsync(api.like(user, tweetId))
      .recover {
        case TwitterError.LikeAlreadyExistError => ()
      }

  def unlike(user: User, tweetId: TweetId): IO[Unit] =
    getIOWithAsync(api.unlike(user, tweetId))
      .recover {
        case TwitterError.LikeNotExistError => ()
      }

  def getTweet(tweetId: TweetId): IO[GetTweetResponse] =
    getIOWithAsync(api.get(tweetId))
      .map(x => GetTweetResponse.found(x))
      .recover {
        case TwitterError.TweetNotExistError => GetTweetResponse.notFound(tweetId)
      }

  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] =
    ids
      .traverse(getTweet)
      .map(x =>
        x.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo]))((acc, e) =>
          e match {
            case GetTweetResponse.NotFound(tweetId) => GetTweetsResponse(acc.notFound + tweetId, acc.found)
            case GetTweetResponse.Found(info)       => GetTweetsResponse(acc.notFound, acc.found + info)
          }
        )
      )
}
