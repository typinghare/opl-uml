class Vehicle {
    public String type;

    public Vehicle(String type) {
        this.type = type;
    }
}

class Car extends Vehicle {
    public String trim;

    public Car(String trim) {
        super("Car");
        this.trim = trim;
    }
}

class Toyota extends Car {
    public Toyota() {
        super("Toyota");
    }
}

public class Main {
    public static void main(String[] args) {
        Vehicle[] arr = new Car[]{new Car("Toyota Camry LX")};
        arr[0] = new Toyota();
        System.out.println(arr[0].type);
    }
}
