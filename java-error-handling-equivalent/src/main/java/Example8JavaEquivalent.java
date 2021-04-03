import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static java.util.Objects.isNull;

public class Example8JavaEquivalent {
    static class Email {
        public final String user;
        public final String domain;

        private Email(String user, String domain) {
            this.user = user;
            this.domain = domain;
        }

        public static Email fromString(String string) throws Exception {
            if (string == null) {
                throw new Exception("Email is null");
            }
            String[] split = string.split("@");
            if (split.length != 2) {
                throw new Exception("Email is malformed. Trying to parse '" + string + "'");
            }
            return new Email(split[0], split[1]);

        }
    }

    static class SSN {
        public final Integer area;
        public final Integer group;
        public final Integer serial;

        private SSN(Integer area, Integer group, Integer serial) {
            this.area = area;
            this.group = group;
            this.serial = serial;
        }

        public static SSN fromString(String string) throws Exception {
            if (isNull(string)) {
                throw new Exception("SSN is null");
            }
            String[] split = string.split("-");
            if (split.length != 3) {
                throw new Exception("Email is malformed. Trying to parse '" + string + "'");
            }
            validateForDigits(string, split);
            validateForInvalidDigits(string, split);

            return new SSN(Integer.valueOf(split[0]), Integer.valueOf(split[1]), Integer.valueOf(split[2]));
        }

        private static void validateForDigits(String string, String[] split) throws Exception {
            List<Character> areaList1 = new ArrayList<>();
            for (char c : split[0].toCharArray()) {
                if (Character.isDigit(c))
                    areaList1.add(c);
            }
            if (areaList1.size() == 0) {
                throw new Exception("No digits found in area position  '" + string + "'");
            }

            List<Character> groupList1 = new ArrayList<>();
            for (char c : split[1].toCharArray()) {
                if (Character.isDigit(c))
                    groupList1.add(c);
            }
            if (groupList1.size() == 0) {
                throw new Exception("No digits found in group position  '" + string + "'");
            }

            List<Character> serialList1 = new ArrayList<>();
            for (char c : split[1].toCharArray()) {
                if (Character.isDigit(c))
                    serialList1.add(c);
            }
            if (serialList1.size() == 0) {
                throw new Exception("No digits found in serial position  '" + string + "'");
            }
        }

        private static void validateForInvalidDigits(String string, String[] split) throws Exception {
            List<Character> areaList2 = new ArrayList<>();
            for (char c : split[0].toCharArray()) {
                if (!Character.isDigit(c))
                    areaList2.add(c);
            }
            if (areaList2.size() != 0) {
                throw new Exception("Invalid digit in area position  '" + string + "'");
            }

            List<Character> groupList2 = new ArrayList<>();
            for (char c : split[1].toCharArray()) {
                if (!Character.isDigit(c))
                    groupList2.add(c);
            }
            if (groupList2.size() != 0) {
                throw new Exception("Invalid digit in group position  '" + string + "'");
            }

            List<Character> serialList2 = new ArrayList<>();
            for (char c : split[1].toCharArray()) {
                if (!Character.isDigit(c))
                    serialList2.add(c);
            }
            if (serialList2.size() != 0) {
                throw new Exception("Invalid digit in serial position  '" + string + "'");
            }
        }


    }

    static class Age {
        public final int age;

        private Age(int age) {
            this.age = age;
        }

        public static Age fromString(String string) throws Exception {
            if (string == null) {
                throw new Exception("Age is null");
            }
            if (string.isEmpty()) {
                throw new Exception("Age is empty");
            }
            List<Character> characterList = new ArrayList<>();
            for (char c : string.toCharArray()) {
                if (Character.isDigit(c))
                    characterList.add(c);
            }
            if (string.length() != characterList.size()) {
                throw new Exception("Age is malformed. trying to parse '" + string + "'");
            }
            return new Age(Integer.valueOf(string));
        }
    }

    public static void main(String[] args) {
        String goodEmail = "mrme@xmail.com";
        String goodSSN = "111-11-2345";
        String goodAge = "49";
        Email goodEmailObject = null;
        String goodEmailError = "";
        try {
            goodEmailObject = Email.fromString(goodEmail);
        } catch (Exception e) {
            goodEmailError = e.getMessage();
        }

        SSN goodSSNObject = null;
        String goodSSNError = "";
        try {
            goodSSNObject = SSN.fromString(goodSSN);
        } catch (Exception e) {
            goodSSNError = e.getMessage();
        }

        Age goodAgeObject = null;
        String goodAgeError = "";
        try {
            goodAgeObject = Age.fromString(goodAge);
        } catch (Exception e) {
            goodAgeError = e.getMessage();
        }

        String goodResult = "";
        String goodResultError = "";

        if (!isNull(goodAgeObject) && !isNull(goodSSNObject) && !isNull(goodAgeObject)) {
            goodResult = String.format("email: %s@%s, ssn: %s-%s-%s, age %s",
                    goodEmailObject.user, goodEmailObject.domain,
                    goodSSNObject.area, goodSSNObject.group, goodSSNObject.serial,
                    goodAgeObject.age);
        } else {
            if (!goodEmailError.isEmpty()) {
                goodResultError = goodEmailError;
            } else if (!goodSSNError.isEmpty()) {
                goodResultError = goodSSNError;
            } else if (!goodAgeError.isEmpty()) {
                goodResultError = goodAgeError;
            }
        }

        String badAge = "old";
        String badSSN = "abc-22-2212";

        Age badAgeObject = null;
        String badAgeError = "";
        try {
            badAgeObject = Age.fromString(badAge);
        } catch (Exception e) {
            badAgeError = e.getMessage();
        }


        SSN badSSNObject = null;
        String badSSNError = "";
        try {
            badSSNObject = SSN.fromString(badSSN);
        } catch (Exception e) {
            badSSNError = e.getMessage();
        }

        String badResult = "";
        String badResultError = "";

        if (!isNull(badAgeObject) && !isNull(badSSNObject) && !isNull(goodEmailObject)) {
            goodResult = String.format("email: %s@%s, ssn: %s-%s-%s, age %s",
                    goodEmailObject.user, goodEmailObject.domain,
                    badSSNObject.area, badSSNObject.group, badSSNObject.serial,
                    badAgeObject.age);
        } else {
            if (!goodEmailError.isEmpty()) {
                badResultError = goodEmailError;
            } else if (!badSSNError.isEmpty()) {
                badResultError = badSSNError;
            } else if (!badAgeError.isEmpty()) {
                badResultError = badAgeError;
            }
        }

        if (badResultError.isEmpty()) {
            System.out.println(badResult);
        } else {
            System.out.println("I know at least one bad thing had happened. And I know what it is: " + badResultError);
        }
        if (goodResultError.isEmpty()) {
            System.out.println(goodResult);
        } else {
            System.out.println("I know at least one bad thing had happened. And I know what it is: " + goodResultError);
        }

        if (badSSNError.isEmpty()) {
            System.out.println(badSSNObject.area + "-" + badSSNObject.group + "-" + badSSNObject.serial);
        } else {
            System.out.println("BAD SSN TRACKER: ERROR: " + badSSNError);
        }

    }
}
