package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import utest._

object PatcherSpec extends TestSuite {

  val tests = Tests {
    "patch simple objects" - {

      case class Foo(a: Int, b: String, c: Double)
      case class Bar(c: Double, a: Int)

      val foo = Foo(0, "", 0.0)
      val bar = Bar(10.0, 10)

      foo.patchUsing(bar) ==>
        Foo(10, "", 10.0)
    }

    "patch with redundant fields" - {

      import TestDomain._

      case class PatchWithRedundantField(phone: Phone, address: String)
      // note address doesn't exist in User

      val patch = PatchWithRedundantField(Phone(4321L), "Unknown")

      compileError("exampleUser.patchUsing(patch)")
        .check(
          "",
          "Derivation failed because target type is missing a field at .address, type in question: io.scalaland.chimney.TestDomain.User, error: .address"
        )

      exampleUser
        .using(patch)
        .ignoreRedundantPatcherFields
        .patch ==>
        exampleUser.copy(phone = patch.phone)
    }

    "support optional types in patch" - {

      import TestDomain._

      case class UserPatch(email: Option[Email], phone: Option[Phone])

      val update = UserPatch(email = Some(Email("updated@example.com")), phone = None)

      exampleUser.patchUsing(update) ==>
        User(10, Email("updated@example.com"), Phone(1234567890L))
    }

    "support mixed optional and regular types" - {

      import TestDomain._

      case class UserPatch(email: Email, phone: Option[Phone])
      val update = UserPatch(email = Email("updated@example.com"), phone = None)

      exampleUser.patchUsing(update) ==>
        User(10, Email("updated@example.com"), Phone(1234567890L))
    }

    "optional fields in the patched object overwritten by None" - {

      import TestDomain._

      case class UserPatch(email: Email, phone: Option[Phone])
      val update = UserPatch(email = Email("updated@example.com"), phone = None)

      exampleUserWithOptionalField.patchUsing(update) ==>
        UserWithOptionalField(10, Email("updated@example.com"), None)
    }

    // I don't think it is an expected behavior and not supporting it for now
    // "fields of type Option[T] in the patched object not overwritten by None of type Option[Option[T]]" - {

    //   import TestDomain._

    //   case class UserWithOptional(id: Int, email: Email, phone: Option[Phone])

    //   case class UserPatch(email: Email, phone: Option[Option[Phone]])
    //   val update = UserPatch(email = Email("updated@example.com"), phone = None)

    //   exampleUserWithOptionalField.patchUsing(update) ==>
    //     UserWithOptionalField(10, Email("updated@example.com"), Some(Phone(1234567890L)))
    // }

    "allow ignoring nones in patches" - {

      import TestDomain._

      case class PhonePatch(phone: Option[Phone])

      exampleUserWithOptionalField.patchUsing(PhonePatch(None)) ==>
        exampleUserWithOptionalField.copy(phone = None)

      exampleUserWithOptionalField
        .using(PhonePatch(None))
        .ignoreNoneInPatch
        .patch ==> exampleUserWithOptionalField

    }

    "allow combining multiple updates in a single patch definition" - {
      import TestDomain._

      case class IdPatch(id: Option[Int])
      case class PhonePatch(phone: Option[Phone])
      case class EmailPatch(id: Option[Int], email: Email)

      exampleUser
        .using(IdPatch(None))
        .and(PhonePatch(Some(Phone(1234567890L))))
        .and(EmailPatch(Some(11), Email("updated@example.com")))
        .patch ==> User(11, Email("updated@example.com"), Phone(1234567890L))

      exampleUserWithOptionalField
        .using(IdPatch(None))
        .and(PhonePatch(None))
        .and(EmailPatch(Some(11), Email("updated@example.com")))
        .patch ==> UserWithOptionalField(11, Email("updated@example.com"), None)
    }

    "allow combining multiple updates in a single patch definition respecting ignoring optional fields" - {
      import TestDomain._

      case class PhonePatch(phone: Option[Phone])
      case class EmailPatch(id: Option[Int], email: Email)

      exampleUserWithOptionalField
        .using(PhonePatch(None))
        .and(EmailPatch(Some(11), Email("updated@example.com")))
        .ignoreNoneInPatch
        .patch ==> exampleUserWithOptionalField.copy(id = 11, email = Email("updated@example.com"))

    }

    "allow combining multiple updates in a single patch definition respecting redundant fields" - {
      import TestDomain._

      case class Name(name: String)
      case class RedundantIdPatch(id: Option[Int], address: String)
      case class RedundantEmailPatch(id: Option[Int], email: Email, name: Name)

      val idPatch = RedundantIdPatch(None, "Moon")
      val emailPatch = RedundantEmailPatch(Some(11), Email("updated@example.com"), Name("Lucas"))

      compileError("exampleUser.using(idPatch).and(emailPatch).patch")
        .check(
          "",
          "Derivation failed because target type is missing a field at .address, type in question: io.scalaland.chimney.TestDomain.User, error: .address"
        )

      compileError("exampleUser.using(emailPatch).and(idPatch).patch")
        .check(
          "",
          "Derivation failed because target type is missing a field at .name, type in question: io.scalaland.chimney.TestDomain.User, error: .name"
        )

      exampleUser
        .using(emailPatch)
        .and(idPatch)
        .ignoreRedundantPatcherFields
        .patch ==> exampleUser.copy(id = 11, email = Email("updated@example.com"))
    }

  }

}

object TestDomain:

  case class Email(address: String) extends AnyVal
  case class Phone(number: Long) extends AnyVal

  case class User(id: Int, email: Email, phone: Phone)
  case class UpdateDetails(email: Email, phone: Phone)

  case class UserWithOptionalField(id: Int, email: Email, phone: Option[Phone])

  val exampleUser = User(10, Email("abc@def.com"), Phone(1234567890L))
  val exampleUserWithOptionalField = UserWithOptionalField(10, Email("abc@def.com"), Option(Phone(1234567890L)))

end TestDomain