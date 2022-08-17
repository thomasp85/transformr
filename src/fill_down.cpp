#include <cpp11/integers.hpp>
#include <cpp11/logicals.hpp>

[[cpp11::register]]
cpp11::integers fill_down(cpp11::logicals nas) {
  int i;
  int last_na = 0;
  cpp11::writable::integers indexes(nas.size());

  for (i = 0; i < nas.size(); ++i) {
    if (!nas[i]) {
      last_na = i + 1;
    }
    indexes[i] = last_na;
  }
  for (i = 0; i < nas.size(); ++i) {
    if (indexes[i] != 0) break;
    indexes[i] = last_na;
  }

  return indexes;
}
