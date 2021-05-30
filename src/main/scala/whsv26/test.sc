
class Foo
class Breathable
class Animal extends Breathable
class Cat extends Animal

val o1: Option[Animal] = Option(new Animal)
val res1 = o1.getOrElse(new Foo)





//trait Thing
//
//class Vehicle extends Thing
//
//class Car extends Vehicle
//
//class Jeep extends Car
//
//class Coupe extends Car
//
//class Motorcycle extends Vehicle
//
//class Bicycle extends Vehicle
//
//class Tricycle extends Bicycle
//
 We need to restrict parking to all subtypes of vehicle, above Tricycle
//
//class Parking[T >: Bicycle <: Vehicle](val plaza: T)
//
//object LowerTypeBounds extends App {
//
   this will compile
//  val parking2 = new Parking[Bicycle](new Bicycle)
//
   this won't compile
//  val parking3 = new Parking[Tricycle](new Tricycle{})
//
}