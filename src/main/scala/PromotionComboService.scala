case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String])

class PromotionComboService {

  /** returns one or none combination for given promotion and promotions set */
  private def getCombo(currentProm: Promotion, available: Seq[Promotion], restricted: Seq[String], combination: Seq[String] = Seq.empty): Seq[String] = available match {
    case Nil => currentProm.code +: combination
    case _ =>
      if (restricted.contains(available.head.code)) getCombo(currentProm, available.tail, restricted, combination)
      else getCombo(currentProm, available.tail, restricted ++ available.head.notCombinableWith, combination :+ available.head.code)
  }

  /** returns all possible cominations for given Promotion and promotion set */
  private def allPossibleCombosForProm(currentProm: Promotion, index: Int, available: Seq[Promotion], map: Map[Seq[String], String]): Map[Seq[String], String] =
    if (index == 0) map else {
      val combo = getCombo(currentProm, available, currentProm.notCombinableWith)
      allPossibleCombosForProm(currentProm, index - 1, available.tail :+ available.head, map ++ Map(combo.sorted -> currentProm.code))
    }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    def combinablePromotionsRec(promotions: Seq[Promotion], map: Map[Seq[String], String] = Map.empty): Map[Seq[String], String] = {
      if (promotions.isEmpty) map else {
        val currentProm = promotions.head
        val availableForCurrentProm = allPromotions.filter(n => !(currentProm.code +: currentProm.notCombinableWith).contains(n.code))
        val currentCombos = allPossibleCombosForProm(currentProm, availableForCurrentProm.size, availableForCurrentProm, map)
        combinablePromotionsRec(promotions.tail, map ++ currentCombos)
      }
    }

    combinablePromotionsRec(allPromotions).keys.map(PromotionCombo).toSeq
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allPromotions.filter(_.code == promotionCode) match {
      case promotion :: Nil =>
        val availableForProm = allPromotions.filter(n => !(promotionCode +: promotion.notCombinableWith).contains(n.code))
        allPossibleCombosForProm(promotion, availableForProm.length, availableForProm, Map.empty).keys.map(PromotionCombo).toSeq
      case _ => Seq.empty[PromotionCombo]
    }
  }
}
