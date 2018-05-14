/*
Copyright 2009-2010 Igor Polevoy 

Licensed under the Apache License, Version 2.0 (the "License"); 
you may not use this file except in compliance with the License. 
You may obtain a copy of the License at 

http://www.apache.org/licenses/LICENSE-2.0 

Unless required by applicable law or agreed to in writing, software 
distributed under the License is distributed on an "AS IS" BASIS, 
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
See the License for the specific language governing permissions and 
limitations under the License. 
*/

package activejdbc.ant.example;

import org.javalite.activejdbc.Base;

/**
 * @author Igor Polevoy
 */
public class Main {
    public static void main(String[] args) {
        Base.open("com.mysql.jdbc.Driver", "jdbc:mysql://localhost/test", "root", "p@ssw0rd");

        createEmployee();
        System.out.println("=========> Created employee:");
        selectEmployee();
        updateEmployee();
        System.out.println("=========> Updated employee:");
        selectAllEmployees();
        deleteEmployee();
        System.out.println("=========> Deleted employee:");
        selectAllEmployees();
        createEmployee();
        System.out.println("=========> Created employee:");
        selectEmployee();
        deleteAllEmployees();
        System.out.println("=========> Deleted all employees:");
        selectAllEmployees();

        Base.close();
    }

    private static void createEmployee() {
        Employee e = new Employee();
        e.set("first_name", "John");
        e.set("last_name", "Doe");
        e.saveIt();
    }

    private static void selectEmployee() {
        Employee e = Employee.findFirst("first_name = ?", "John");
        System.out.println(e);
    }

    private static void updateEmployee() {
        Employee e = Employee.findFirst("first_name = ?", "John");
        e.set("last_name", "Steinbeck").saveIt();
    }

    private static void deleteEmployee() {
        Employee e = Employee.findFirst("first_name = ?", "John");
        e.delete();
    }

    private static void deleteAllEmployees() {
            Employee.deleteAll();
    }

    private static void selectAllEmployees() {
            System.out.println("Employees list: " + Employee.findAll());
    }
}