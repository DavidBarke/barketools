#include <Rcpp.h>
#include <string>
#include <cctype>

using namespace Rcpp;

// Function to calculate the check digit
char calculateCheckDigit(const std::string& cusip) {
  int sum = 0;
  for (size_t i = 0; i < cusip.length(); ++i) {
    char c = cusip[i];
    int value;

    if (std::isdigit(c)) {
      value = c - '0'; // Convert char to int (0-9)
    } else if (std::isalpha(c)) {
      value = c - 'A' + 10; // Convert 'A'-'Z' to 10-35
    } else {
      Rcpp::stop("CUSIP contains invalid characters.");
    }

    // Weight the values
    if (i % 2 == 0) { // Even index
      sum += value;
    } else { // Odd index
      int doubleValue = value * 2;
      sum += doubleValue / 10 + doubleValue % 10; // Split the digits
    }
  }

  // Compute the check digit
  int checkDigit = (10 - (sum % 10)) % 10;
  return '0' + checkDigit; // Convert back to char
}

//' Map 8-digit CUSIP to 9-digit CUSIP
//'
//' This function computes the check digit and adds it to the 8-digit CUSIP.
//'
//' @param cusip8 A character vector of 8-digit CUSIPs.
//'
//' @export
// [[Rcpp::export]]
CharacterVector cusip8_to_cusip9(CharacterVector cusip8) {
  int n = cusip8.size();
  CharacterVector cusip9_vector(n);

  for (int i = 0; i < n; ++i) {
    std::string cusip8_str = Rcpp::as<std::string>(cusip8[i]);

    if (cusip8_str.length() != 8) {
      Rcpp::stop("CUSIP must be 8 characters long.");
    }

    // Append a '0' to the end
    std::string cusip9_str = cusip8_str + '0';

    // Calculate the check digit and replace the last character
    char checkDigit = calculateCheckDigit(cusip9_str);
    cusip9_str[8] = checkDigit;

    cusip9_vector[i] = cusip9_str; // Store the result
  }

  return cusip9_vector;
}
