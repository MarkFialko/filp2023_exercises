package competition

import competition.domain.ScenarioError.TopAuthorNotFound
import service.TwitterService
import twitter.domain.User

import scala.concurrent.{ExecutionContext, Future}

class FutureCompetition(service: TwitterService[Future], methods: CompetitionMethods[Future])(
    implicit ec: ExecutionContext
) extends Competition[Future] {
  def winner(users: List[User], followers: Map[User, List[User]], botUser: User): Future[User] =
    for {
      tweetsLike <- Future.traverse(users)(user =>
        for {
          tweet <- service.tweet(user, s"${user.id} will win!")
          _     <- Future.sequence(followers(user).map(x => service.like(x, tweet)))
        } yield tweet
      )
      _         <- methods.unlikeAll(botUser, tweetsLike)
      topAuthor <- methods.topAuthor(tweetsLike)
      result <- topAuthor match {
        case Some(value) => Future.successful(value)
        case None        => Future.failed(TopAuthorNotFound)
      }
    } yield result
}

object FutureCompetitionStart extends App {
  import scala.util.Random
  import scala.concurrent.duration.DurationInt
  import scala.concurrent.Await
  import twitter.{LocalTwitterApi, TwitterApi}
  import _root_.service.TwitterServiceFuture

  implicit val ec: ExecutionContext = ExecutionContext.global

  val api: TwitterApi = new LocalTwitterApi(Iterator.continually((Random.nextDouble() * 1000).toInt))

  val service: TwitterService[Future] = new TwitterServiceFuture(api)

  val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

  val oleg: User   = User("oleg")
  val ivan: User   = User("ivan")
  val marya: User  = User("marya")
  val gustav: User = User("gustav")
  val bot: User    = User("bot")

  val users: List[User] = List(oleg, ivan, marya, gustav, bot)

  val followers: Map[User, List[User]] = Map(
    oleg   -> List(ivan, bot),
    ivan   -> List(oleg, gustav),
    marya  -> List(oleg, ivan, gustav, bot),
    gustav -> List(oleg, ivan, marya),
    bot    -> List(bot)
  )

  private val winner: User =
    Await.result(new FutureCompetition(service, methods).winner(users, followers, bot), 30.seconds)
  println(s"${winner.id} win!!!")
}
