open Bignum
open CS51Utils
open Absbook

type bignum_io = { raw : int; bn : bignum }

type bignum_samples_type = {
  zero : bignum_io;
  positive : bignum_io;
  pos_reverse : bignum_io;
  negative : bignum_io;
  neg_reverse : bignum_io;
  small_pos : bignum_io;
  small_neg : bignum_io;
  big_pos : bignum_io;
  overflow_pos : bignum_io;
  overflow_neg : bignum_io;
  one : bignum_io;
  sumof_pos_neg : bignum_io;
  sumof_pos_rev_neg : bignum_io;
  sumof_pos_neg_rev : bignum_io;
  sumof_neg_neg : bignum_io;
  sumof_big_small_pos : bignum_io;
  prodof_pos_pos : bignum_io;
  prodof_pos_neg : bignum_io;
  prodof_neg_neg : bignum_io;
}

let bn_s : bignum_samples_type =
  {
    zero = { raw = 0; bn = { neg = false; coeffs = [] } };
    positive =
      { raw = 9123456789; bn = { neg = false; coeffs = [ 9; 123; 456; 789 ] } };
    pos_reverse =
      { raw = 9876543219; bn = { neg = false; coeffs = [ 9; 876; 543; 219 ] } };
    negative =
      { raw = ~-9123456789; bn = { neg = true; coeffs = [ 9; 123; 456; 789 ] } };
    neg_reverse =
      { raw = ~-9876543219; bn = { neg = true; coeffs = [ 9; 876; 543; 219 ] } };
    small_pos = { raw = 5; bn = { neg = false; coeffs = [ 5 ] } };
    small_neg = { raw = ~-5; bn = { neg = true; coeffs = [ 5 ] } };
    big_pos =
      {
        raw = 987000321123456789;
        bn = { neg = false; coeffs = [ 987; 000; 321; 123; 456; 789 ] };
      };
    overflow_pos =
      {
        raw = 7;
        (* I don't want to introduce option types to these. This field should never be referenced. *)
        bn = { neg = false; coeffs = [ 9; 876; 543; 210; 000; 123; 456; 789 ] };
      };
    overflow_neg =
      {
        raw = ~-7;
        (* I don't want to introduce option types to these. This field should never be referenced. *)
        bn = { neg = true; coeffs = [ 9; 876; 543; 210; 000; 123; 456; 789 ] };
      };
    one = { raw = 1; bn = { neg = false; coeffs = [ 1 ] } };
    sumof_pos_neg = { raw = 0; bn = { neg = false; coeffs = [] } };
    sumof_pos_rev_neg =
      { raw = 753086430; bn = { neg = false; coeffs = [ 753; 086; 430 ] } };
    sumof_pos_neg_rev =
      { raw = ~-753086430; bn = { neg = true; coeffs = [ 753; 086; 430 ] } };
    sumof_neg_neg =
      {
        raw = ~-18246913578;
        bn = { neg = true; coeffs = [ 18; 246; 913; 578 ] };
      };
    sumof_big_small_pos =
      {
        raw = 987000321123456794;
        bn = { neg = false; coeffs = [ 987; 000; 321; 123; 456; 794 ] };
      };
    prodof_pos_pos =
      {
        raw = 9123456789 * 5;
        bn = { neg = false; coeffs = [ 45; 617; 283; 945 ] };
      };
    prodof_pos_neg =
      {
        raw = 5 * ~-9123456789;
        bn = { neg = true; coeffs = [ 45; 617; 283; 945 ] };
      };
    prodof_neg_neg =
      {
        raw = ~-9123456789 * ~-5;
        bn = { neg = false; coeffs = [ 45; 617; 283; 945 ] };
      };
  }

let negate_test () =
  unit_test (negate bn_s.zero.bn = bn_s.zero.bn) "\n🔖 negate: 0 is unchanged";
  unit_test
    (negate bn_s.positive.bn = bn_s.negative.bn)
    "🔖 negate: positive to negative";
  unit_test
    (negate bn_s.negative.bn = bn_s.positive.bn)
    "🔖 negate: negative to positive"

let equal_test () =
  unit_test (equal bn_s.zero.bn bn_s.zero.bn) "\n🔖 equal: 0 equals 0";
  unit_test (equal bn_s.negative.bn bn_s.negative.bn) "🔖 equal: equal neg ints";
  unit_test
    (not (equal bn_s.zero.bn bn_s.positive.bn))
    "🔖 equal: 0 not equal positive int";
  unit_test
    (not (equal bn_s.positive.bn bn_s.negative.bn))
    "🔖 equal: same value pos / neg";
  unit_test
    (not (equal bn_s.positive.bn bn_s.pos_reverse.bn))
    "🔖 equal: reversed nums not equal";
  unit_test
    (not (equal bn_s.small_pos.bn bn_s.big_pos.bn))
    "🔖 equal: short v long"

let less_test () =
  unit_test (not (less bn_s.zero.bn bn_s.zero.bn)) "\n🔖 less: 0 < 0";
  unit_test
    (not (less bn_s.negative.bn bn_s.negative.bn))
    "🔖 less: equal neg ints";
  unit_test (less bn_s.zero.bn bn_s.positive.bn) "🔖 less: 0 < positive int";
  unit_test
    (not (less bn_s.positive.bn bn_s.negative.bn))
    "🔖 less: pos not < neg";
  unit_test
    (less bn_s.positive.bn bn_s.pos_reverse.bn)
    "🔖 less: small pos < big pos";
  unit_test (less bn_s.negative.bn bn_s.zero.bn) "🔖 less: neg < 0";
  unit_test
    (less bn_s.neg_reverse.bn bn_s.negative.bn)
    "🔖 less: big neg < small neg"

let greater_test () =
  unit_test (not (greater bn_s.zero.bn bn_s.zero.bn)) "\n🔖 greater: 0 not > 0";
  unit_test
    (not (greater bn_s.negative.bn bn_s.negative.bn))
    "🔖 greater: equal neg ints";
  unit_test
    (greater bn_s.positive.bn bn_s.zero.bn)
    "🔖 greater: positive int > 0";
  unit_test
    (not (greater bn_s.negative.bn bn_s.positive.bn))
    "🔖 greater: neg not < pos";
  unit_test
    (not (greater bn_s.positive.bn bn_s.pos_reverse.bn))
    "🔖 greater: small pos not < big pos";
  unit_test (greater bn_s.zero.bn bn_s.negative.bn) "🔖 greater: 0 > neg";
  unit_test
    (greater bn_s.negative.bn bn_s.neg_reverse.bn)
    "🔖 greater: small neg > big neg"

let from_int_test () =
  unit_test (from_int bn_s.zero.raw = bn_s.zero.bn) "\n🔖 from_int: convert 0";
  unit_test
    (from_int bn_s.positive.raw = bn_s.positive.bn)
    "🔖 from_int: convert positive";
  unit_test
    (from_int bn_s.negative.raw = bn_s.negative.bn)
    "🔖 from_int: convert negative";
  unit_test
    (from_int bn_s.small_pos.raw = bn_s.small_pos.bn)
    "🔖 from_int: convert small";
  unit_test
    (from_int bn_s.big_pos.raw = bn_s.big_pos.bn)
    "🔖 from_int: convert large"

let to_int_test () =
  unit_test (to_int bn_s.zero.bn = Some bn_s.zero.raw) "\n🔖 to_int: convert 0";
  unit_test
    (to_int bn_s.positive.bn = Some bn_s.positive.raw)
    "🔖 to_int: convert positive";
  unit_test
    (to_int bn_s.negative.bn = Some bn_s.negative.raw)
    "🔖 to_int: convert negative";
  unit_test
    (to_int bn_s.small_pos.bn = Some bn_s.small_pos.raw)
    "🔖 to_int: convert small";
  unit_test
    (to_int bn_s.big_pos.bn = Some bn_s.big_pos.raw)
    "🔖 to_int: convert large";
  unit_test (to_int bn_s.overflow_pos.bn = None) "🔖 to_int: overflow pos";
  unit_test (to_int bn_s.overflow_neg.bn = None) "🔖 to_int: overflow neg"

let plus_test () =
  unit_test
    (plus bn_s.zero.bn bn_s.zero.bn = bn_s.zero.bn)
    "\n🔖 plus: zero + zero";
  unit_test
    (plus bn_s.positive.bn bn_s.negative.bn = bn_s.sumof_pos_neg.bn)
    "🔖 plus: positive + negative";
  unit_test
    (plus bn_s.pos_reverse.bn bn_s.negative.bn = bn_s.sumof_pos_rev_neg.bn)
    "🔖 plus: positive reverse + negative";
  unit_test
    (plus bn_s.positive.bn bn_s.neg_reverse.bn = bn_s.sumof_pos_neg_rev.bn)
    "🔖 plus: positive + negative reverse";
  unit_test
    (plus bn_s.negative.bn bn_s.negative.bn = bn_s.sumof_neg_neg.bn)
    "🔖 plus: negative + negative";
  unit_test
    (plus bn_s.big_pos.bn bn_s.small_pos.bn = bn_s.sumof_big_small_pos.bn)
    "🔖 plus: big + small pos"

let times_test () =
  unit_test
    (times bn_s.zero.bn bn_s.positive.bn = bn_s.zero.bn)
    "\n🔖 times: prod of 0";
  unit_test
    (times bn_s.big_pos.bn bn_s.one.bn = bn_s.big_pos.bn)
    "🔖 times: identity";
  unit_test
    (times bn_s.positive.bn bn_s.small_pos.bn = bn_s.prodof_pos_pos.bn)
    "🔖 times: pos * pos";
  unit_test
    (times bn_s.small_pos.bn bn_s.negative.bn = bn_s.prodof_pos_neg.bn)
    "🔖 times: pos * neg";
  unit_test
    (times bn_s.negative.bn bn_s.small_neg.bn = bn_s.prodof_neg_neg.bn)
    "🔖 times: neg * neg"

let test () =
  negate_test ();
  equal_test ();
  less_test ();
  greater_test ();
  from_int_test ();
  to_int_test ();
  plus_test ();
  times_test ()
;;

test ()

;;