import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class PromotionComboServiceSpec extends AnyFlatSpec with Matchers {

  val service = new PromotionComboService

  //test data
  private val testPromotion = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
  )

  private val testNoCombinationsPossible = Seq(Promotion("P1", Seq("P3", "P2")), Promotion("P2", Seq("P1", "P3")), Promotion("P3", Seq("P1", "P2")))

  "PromotionComboService" should
    "return all possible promotion combos for given promotion set" in {
    val expectedResult = Seq(PromotionCombo(List("P1", "P2")),
      PromotionCombo(List("P1", "P4", "P5")),
      PromotionCombo(List("P2", "P3")),
      PromotionCombo(List("P3", "P4", "P5")))

    service.allCombinablePromotions(testPromotion) mustBe expectedResult
  }

  it should "return empty Seq if no promotions passed as a parameter" in {
    service.allCombinablePromotions(Seq.empty) mustBe Seq.empty
  }

  it should "return empty Seq if no combinations possible" in {
    service.allCombinablePromotions(testNoCombinationsPossible) mustBe Seq.empty
  }

  it should "return combination of all promotions for promotions with no restrictions" in {
    val testNoRestrictionPromos = Seq(Promotion("P1", Seq.empty), Promotion("P2", Seq.empty), Promotion("P3", Seq.empty))

    service.allCombinablePromotions(testNoRestrictionPromos) mustBe Seq(PromotionCombo(List("P1", "P2", "P3")))
  }

  it should "return all combinations for promotion code P1" in {
    service.combinablePromotions("P1", testPromotion) mustBe Seq(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5")))

  }
  it should "return all combinations for promotion code P3" in {
    service.combinablePromotions("P3", testPromotion) mustBe Seq(PromotionCombo(List("P2", "P3")), PromotionCombo(List("P3", "P4", "P5")))
  }

  it should "return empty Seq when no combinations possible for given promotion code" in {
    service.combinablePromotions("P1", testNoCombinationsPossible) mustBe Seq.empty
  }

  /** assuming that if promotions is not to be found, the default behaviour would be to not join it with any other */
  it should "return empty Seq when promotion code not found in given promotions" in {
    service.combinablePromotions("P8", testPromotion) mustBe Seq.empty
  }

  it should "return empty Seq for empty string passed for promotion code " in {
    service.combinablePromotions("", testPromotion) mustBe Seq.empty
  }

  it should "return empty Seq when no promotions passed" in {
    service.combinablePromotions("P1", Seq.empty) mustBe Seq.empty
  }

  it should "return empty Seq when empty string for promotion code and no promotions passed" in {
    service.combinablePromotions("", Seq.empty) mustBe Seq.empty
  }
}
