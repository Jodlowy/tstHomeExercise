import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class GroupPriceServiceSpec extends AnyFlatSpec with Matchers {

  val service = new GroupPriceService

  private val testRates = Seq(Rate("M1", "Military"), Rate("M2", "Military"), Rate("S1", "Senior"), Rate("S2", "Senior"))

  private val testCabinPrices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00))

  private val testStandardCabinPrices = Seq(
    CabinPrice("CA", "St1", 200.00),
    CabinPrice("CA", "St2", 250.00),
    CabinPrice("CB", "St1", 270.00))

  "GroupPriceService" should
    "return best prices for given group rates" in {
    val expectedResult = Seq(
      BestGroupPrice("CA", "M1", 200.0, "Military"),
      BestGroupPrice("CA", "S1", 225.0, "Senior"),
      BestGroupPrice("CB", "M1", 230.0, "Military"),
      BestGroupPrice("CB", "S1", 245.0, "Senior"))

    service.getBestGroupPrices(testRates, testCabinPrices) mustBe expectedResult
  }

  it should "return single best prices when duplicates occur" in {
    val testDuplicateCabinPrices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "S1", 250.00),
      CabinPrice("CA", "S1", 250.00),
      CabinPrice("CA", "M1", 230.00))

    val expectedResult = Seq(BestGroupPrice("CA", "M1", 200.0, "Military"), BestGroupPrice("CA", "S1", 250.0, "Senior"))

    service.getBestGroupPrices(testRates, testDuplicateCabinPrices) mustBe expectedResult
  }

  it should "return empty Seq when no rates matches cabin prices" in {
    val testStandardRates = Seq(Rate("St1", "Standard"), Rate("St", "Standard"))

    service.getBestGroupPrices(testStandardRates, testCabinPrices) mustBe Seq.empty
  }

  it should "return empty Seq when no cabin prices matches rates" in {
    service.getBestGroupPrices(testRates, testStandardCabinPrices) mustBe Seq.empty
  }

  it should "return empty Seq when empty Seq passed for rates" in {
    service.getBestGroupPrices(Seq.empty, testStandardCabinPrices) mustBe Seq.empty
  }

  it should "return empty Seq when empty Seq passed for cabin prices" in {
    service.getBestGroupPrices(testRates, Seq.empty) mustBe Seq.empty
  }

  it should "return empty Seq for both parameters passed as empty" in {
    service.getBestGroupPrices(Seq.empty, Seq.empty) mustBe Seq.empty
  }

  it should "ignore cabin prices with empty rate code" in {
    val testBestCabinPricesWithCorruptedData = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "qwerty", 0),
      CabinPrice("CA", "", 0),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00)
    )

    val expectedResult = Seq(BestGroupPrice("CA", "M1", 200.0, "Military"), BestGroupPrice("CA", "S1", 225.0, "Senior"), BestGroupPrice("CB", "M1", 230.0, "Military"))

    service.getBestGroupPrices(testRates, testBestCabinPricesWithCorruptedData) mustBe expectedResult
  }

  it should "ignore rates with empty rate code " in {
    val testCorruptedRates = Seq(Rate("", "Military"), Rate("M2", "Military"), Rate("qwerty", "Senior"), Rate("S2", "Senior"))

    val expectedResult = Seq(
      BestGroupPrice("CA", "M2", 250.0, "Military"),
      BestGroupPrice("CB", "M2", 260.0, "Military"),
      BestGroupPrice("CA", "S2", 260.0, "Senior"),
      BestGroupPrice("CB", "S2", 270.0, "Senior"))

    service.getBestGroupPrices(testCorruptedRates, testCabinPrices) mustBe expectedResult
  }
}
