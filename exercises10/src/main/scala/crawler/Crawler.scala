package crawler

import cats.effect.{ContextShift, IO}
import cats.implicits._

import scala.util.matching.Regex

/*
 * В данной работе необходимо написать web-crawler.
 *
 * Crawler - это программа, которая анализует структуру данного ей сайта.
 * Эта программа посещает все ссылки, которые ей удаётся найти на заданном ей ресурсе.
 *
 * Вам необходимо реализовать функцию `crawl`.
 *
 * Эта функция будет вызвана с некоторым урлом. Например: "http://wikipedia.org"
 * Задача этой функции:
 *   1. Посетить переданный ей урл.
 *   2. Обработать ответ формата html - получив новые урлы для посещения.
 *   3. Посетить полученные из ответа урлы, в случае если они принадлежат тому же домену (поддомены не считать, только строгое совпадение).
 *   4. Для каждого урла, из шага 3, повторять все с 1 шага до тех пор, пока Crawler не посетит все страницы
 *   5. Вернуть набор посещённых страниц
 *
 * Таким образом, программа должна посетить каждую страницу сайта, до которой сможет добраться через html ответы.
 *
 * При этом, должны соблюдаться условия:
 *   - Каждая страница должна быть посещена строго 1 раз.
 *   - Ответ HttpClient может вернуть ошибку (завершение IO с Exception). В этом случае, необходимо попытаться
 *     повторить запрос, но не более 3 раз. Если после 3 запросов всё ещё возвращается ошибка,
 *     урл необходимо пропустить (урла не должно быть в результате функции).
 *   - Нельзя покидать сайт. Скажем если на сайте "http://wikipedia.org" вы вдруг найдёт ссылку на сторонний ресурс,
 *     её необходимо пропустить (её нельзя вызывать, урла не должно быть в результате функции).
 *
 * Прочие условности:
 *   - Пример html ответа
 *       <html><body>
 *         <div>some hmtl here</div>
 *         <div>quite <p style="color: red">natural</p></div>
 *         <div><a href="/page1">first page</a></div>
 *       </body></html>
 *     Таким образом, следует считать что html ответ приближен к реальности.
 *   - Искомые ссылки находятся в атрибуте `href`
 *   - На странице может быть 0+ ссылок. Не обязательно одна.
 *   - Поиск по html можете производить любым удобным способом.
 *     Например, можете использовать UrlSearch.search
 *   - Учитывайте, что URL адреса могут быть как абсолютными так и относительными.
 *   - В идеале, обработку ссылок распараллелить - для этого Crawler'у предоставляется ContextShift.
 *   - Обычно, в реальном мире, перед тем как совершить повторный запрос, вы бы выжидали какое-то время.
 *     Возможно даже это время увеличивалось бы с каждый запросом согласно какой-то функции.
 *     Однако в данной работе, не нужно выжидать и делать паузы. Именно по этой причине вам не предоставлен Timer.
 *     Получив ошибку, необходимо сразу же повторить запрос.
 *   - Можете определять доменную модель, как считаете нужным.
 */
class Crawler(client: HttpClient[IO])(implicit cs: ContextShift[IO]) {
  def crawl(root: HttpClient.URL): IO[Set[HttpClient.URL]] = {
    def crawl_inner(
        urls: List[HttpClient.URL],
        visited: Set[HttpClient.URL],
        banned: Set[HttpClient.URL],
        attempt: Integer
    ): IO[Set[HttpClient.URL]] = {
      if (attempt < 3) {
        IO(
          urls.headOption match {
            case None => visited
            case Some(x) =>
              client
                .get(x)
                .map(r => UrlSearch.search(root, x, r))
                .flatMap(inner_links =>
                  crawl_inner(
                    (inner_links.toList ::: urls).toSet.diff(visited + x).diff(banned).toList,
                    visited + x,
                    banned,
                    0
                  )
                )
                .unsafeRunSync()
          }
        ).handleErrorWith(_ => crawl_inner(urls, visited, banned, attempt + 1))
      } else {
        urls match {
          case h :: t => crawl_inner(t, visited, banned + h, 0)
        }
      }
    }

    crawl_inner(List(root), Set(root), Set(), 0)
  }

}
