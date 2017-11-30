package edu.umass.cs.iesl.watr
package watrcolors

import persistence._
import models.users._
import TypeTags._
// import cats._
// import cats.effect._

class PersistenceTests extends DatabaseTest {
  behavior of "User/Token/Password Persistence tests"


  behavior of "UserStore"

  it should "put/get/update/delete" in new EmptyDatabase {

    val store = new UserStore(reflowDB)

    store.put(User(UserID(0), EmailAddr("a@b.c")))

    val user = store
      .put(User(UserID(0), EmailAddr("a@b.c")))
      .unsafeRunSync()

    val u0 = store.get(user.id.unwrap)
      .getOrElse { fail }
      .unsafeRunSync()

    assert( user === u0 )

    // val userUp = store.update(user.copy(email=EmailAddr("x@y.z")))
    //   .unsafeRunSync()

    // val userUp0 = store.get(userUp.id.unwrap)
    //   .getOrElse { fail }
    //   .unsafeRunSync()

    // assert(userUp === userUp0)
  }

  behavior of "TokenStore"
  import tsec.authentication.AuthEncryptedCookie
  import tsec.cipher.symmetric.imports.AES128

  it should "put/get/update/delete" in new EmptyDatabase {

    val userStore = new UserStore(reflowDB)

    val user = userStore
      .put(User(UserID(0), EmailAddr("a@b.c")))
      .unsafeRunSync()

    val cookie = AuthEncryptedCookie(
      id=java.util.UUID.randomUUID(),
      name="my-name",
      content=tsec.cookies.AEADCookie[AES128]("my-content"),
      identity=user.id.unwrap,
      expiry=null,
      lastTouched=None,
      secure=true,
      httpOnly=true,
      domain = None,
      path = None,
      extension = None
    )

    val tokenStore = new TokenStore(reflowDB)

    val cookie0 = tokenStore
      .put(cookie)
      .unsafeRunSync()

    // cookie0.name == "my-name"
    assert(cookie0.name === "my-name")

    // put should update fields
    val cookie1 = tokenStore
      .put(cookie.copy(name="other-name"))
      .unsafeRunSync()

    assert(cookie1.name == "other-name")
  }

  behavior of "PasswordStore"

  // import tsec.passwordhashers.imports.SCrypt
  import tsec.passwordhashers._
  import tsec.passwordhashers.imports._

  it should "put/get/update/delete" in new EmptyDatabase {
    val passwordStore = new PasswordStore(reflowDB)
    val userStore = new UserStore(reflowDB)

    val user = userStore
      .put(User(UserID(0), EmailAddr("a@b.c")))
      .unsafeRunSync()

    val hashedPass = "my-password".hashPassword[SCrypt]

    val authInfo = passwordStore.put(AuthInfo(
      user.id,
      Username("my-username"),
      hashedPass
    )).unsafeRunSync()

    val authInfo2 = passwordStore.put(
      authInfo.copy(username = Username("my-other-name"))
    ).unsafeRunSync()


    assert( authInfo.username === "my-username" )
    assert( authInfo2.username === "my-other-name" )

    val mypass = SCrypt.fromString(hashedPass)

    SCrypt.checkPassword(core.Password("my-password"), mypass)

  }
}
