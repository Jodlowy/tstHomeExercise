case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String) extends Ordered[BestGroupPrice] {
  def compare(that: BestGroupPrice): Int = this.price compare that.price
}

class GroupPriceService {

  private def joinRateWithCabinPrice(rates: Seq[Rate], cabPrices: Seq[CabinPrice]): Seq[BestGroupPrice] = for { //O(nm)
    rate <- rates
    cPrice <- cabPrices
    if rate.rateCode == cPrice.rateCode
  } yield BestGroupPrice(cPrice.cabinCode, cPrice.rateCode, cPrice.price, rate.rateGroup)

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] =
    joinRateWithCabinPrice(rates, prices).groupBy(gP => (gP.cabinCode, gP.rateGroup)).values.map(_.min).toSeq.sortBy(_.price)
}

