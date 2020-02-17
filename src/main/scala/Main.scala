
object Main extends App {

  val rates = Seq(Rate("M1", "Military"), Rate("M2", "Military"), Rate("S1", "Senior"), Rate("S2", "Senior"))

  val cabinPrices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00))

  val promoSeq = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
  )

  val groupPriceService = new GroupPriceService
  val promotionComboService = new PromotionComboService

  println("Best Cabin Prices:")
  println(groupPriceService.getBestGroupPrices(rates, cabinPrices))
  println

  println("Output for All Promotion Combinations:")
  println(promotionComboService.allCombinablePromotions(promoSeq))
  println

  println("Output for Promotion Combinations for promotionCode=”P1”:")
  println(promotionComboService.combinablePromotions("P1", promoSeq))
  println

  println("Output for Promotion Combinations for promotionCode=”P3”:")
  println(promotionComboService.combinablePromotions("P3", promoSeq))
}
