#include <iostream>
#include <sstream>
#include <iomanip>
#include <fstream>
#include <algorithm>
#include <string>
#include <map>
#include <vector>
#include <math.h>
#include <chrono>

typedef std::vector<bool> sequence;

std::string humanize_seconds(double seconds) {
  std::ostringstream result;
  if (seconds >= 86400) {
    result << (int) seconds / 86400 << " days, " << ((int) seconds % 86400) / 3600 << " hours";
  } else if (seconds >= 3600) {
    result << (int) seconds / 3600 << " hours, " << ((int) seconds % 3600) / 60 << " minutes";
  } else if (seconds >= 60) {
    result << (int) seconds / 60 << " minutes, " << (int) seconds % 60 << " seconds";
  } else if (seconds >= 0) {
    result << (int) seconds << " seconds";
  } else {
    result << "unknown time";
  }
  return result.str();
}

void write_to_file(std::ofstream &file, const sequence &new_seq) {
  for (auto it = new_seq.begin(); it != new_seq.end(); it++) {
    file << *it;
  }
  file << std::endl;
}

void write_to_file(std::ofstream &file, int n, int m, const sequence &new_seq, long found_seqs, long pair_index, long search_space_size) {
  for (auto it = new_seq.begin(); it != new_seq.end(); it++) {
    file << *it;
  }
  file << "," << found_seqs << "," << pair_index << std::endl;
}

int binom(int n, int k) {
  double res = 1;
  for (int i = 1; i <= k; ++i)
      res = res * (n - k + i) / i;
  return (int)(res + 0.01);
}

int atom_count(int m) {
  return binom(2 * m, m);
}

std::vector<sequence> atoms(int m) {
  std::vector< std::vector<bool> > res;

  std::vector<bool> v(2 * m);
  std::fill(v.end() - m, v.end(), true);

  for (int i = 0; i < atom_count(m); i++) {
    res.push_back(v);
    std::next_permutation(v.begin(), v.end());
  }

  return res;
}

std::vector<sequence> nested_marvellous(int n, int m, long random_sample_bound, bool print_to_file) {
  if (n == 1) {
    std::cout << "-- n = 1" << std::endl;
    std::cout << "   Sequence size: " << 2 * m << std::endl;
    auto ret = atoms(m);
    std::cout << "   Done" << std::endl;
    std::cout << "   " << ret.size() << " sequences found" << std::endl;

    if (print_to_file) {
      std::ofstream output_file;
      output_file.open("1-" + std::to_string(m) + ".txt");
      for (auto it = ret.begin(); it != ret.end(); it++) {
        write_to_file(output_file, *it);
      }
      output_file.close();
    }

    return ret;
  } else if (n > 1) {
    auto prev_lev_seqs = nested_marvellous(n - 1, m, random_sample_bound, print_to_file);
    std::vector<sequence> ret;

    long search_space_size = pow(prev_lev_seqs.size(), 2);
    std::cout << "-- n = " << n << std::endl;
    std::cout << "   Sequence size: " << m * pow(2, n) << std::endl;
    std::cout << "   Search space size: " << search_space_size << std::endl;

    // Precompute pattern count for previous level
    std::vector< std::vector< int > > prev_lev_patt_count(prev_lev_seqs.size(), std::vector< int >(pow(2, n), 0));
    for (long i = 0; i < prev_lev_seqs.size(); i++) {
      auto seq = prev_lev_seqs[i];
      for (int j = 0; j < seq.size() - n + 1; j++) {
        int patt_index = 0;
        for (int k = 0; k < n; k++) {
          if (seq[j + k]) {
            patt_index += pow(2, n - k - 1);
          }
        }
        prev_lev_patt_count[i][patt_index]++;
      }
    }
    std::cout << "   Patterns precomputed for previous level sequences" << std::endl;

    std::cout << "   Testing sequence pairs" << std::endl;
    
    // Determine if random sampling has to be used
    bool using_random_sampling = false;
    if (random_sample_bound > 0 && search_space_size > random_sample_bound) {
      using_random_sampling = true;
      std::cout << "   Random sampling will be used (bound: " << random_sample_bound << ")" << std::endl;
    }

    // Create file for writing out results
    std::ofstream output_file;
    if (print_to_file) {
      output_file.open(std::to_string(n) + "-" + std::to_string(m) + (using_random_sampling ? "-rand.txt" : ".txt"));
    }
    
    // Now check every possible pair
    long pair_index = 0;
    auto start_time = std::chrono::high_resolution_clock::now();
    for (long i1 = 0; i1 < prev_lev_seqs.size(); i1++) {
      for (long i2 = 0; i2 < prev_lev_seqs.size(); i2++) {
        // Random sample. We keep the for-structure for simplicity.
        if (using_random_sampling) {
          i1 = rand() % prev_lev_seqs.size();
          i2 = rand() % prev_lev_seqs.size();
        }

        auto seq1 = prev_lev_seqs[i1];
        auto seq2 = prev_lev_seqs[i2];
        std::vector< int > joint_patt_count(pow(2, n), 0);
        for (int j = 0; j < n - 1; j ++) {
          int patt_index = 0;
          for (int k = 0; k < n; k++) {
            bool elem = j + k < n - 1 ? seq1[seq1.size() + j + k - n + 1] : seq2[j + k - n + 1];
            if (elem) {
              patt_index += pow(2, n - k - 1);
            }
          }
          joint_patt_count[patt_index]++;

          patt_index = 0;
          for (int k = 0; k < n; k++) {
            bool elem = j + k < n - 1 ? seq2[seq2.size() + j + k - n + 1] : seq1[j + k - n + 1];
            if (elem) {
              patt_index += pow(2, n - k - 1);
            }
          }
          joint_patt_count[patt_index]++;
        }
        // Check maravillosidad
        bool is_marvellous = true;
        for (int j = 0; j < pow(2, n); j++) {
          is_marvellous = (prev_lev_patt_count[i1][j] + prev_lev_patt_count[i2][j] + joint_patt_count[j] == m) && is_marvellous;
        }
        if (is_marvellous) {
          sequence new_seq;
          new_seq.reserve(2 * seq1.size());
          new_seq.insert(new_seq.end(), seq1.begin(), seq1.end());
          new_seq.insert(new_seq.end(), seq2.begin(), seq2.end());
          ret.push_back(new_seq);
          if (print_to_file) {
            write_to_file(output_file, n, m, new_seq, ret.size(), pair_index, search_space_size);
          }
        }
        if (pair_index % 5000 == 0) {
          auto current_time = std::chrono::high_resolution_clock::now();
          std::chrono::duration<double> elapsed_time = current_time - start_time;
          double progress = (double) (pair_index) / (using_random_sampling ? random_sample_bound : search_space_size);
          double remaining_seconds = elapsed_time.count() * (1 - progress) / progress;
          std::cout << "\r   " << ret.size() << " sequences found, " << pair_index << " (" << (100 * progress) << "\%) examined, " << humanize_seconds(remaining_seconds) << " remaining";
        }
        pair_index++;

        // Take only some sequences (use with random sample)
        if (using_random_sampling && pair_index > random_sample_bound) {
          break;
        }
      }
      if (using_random_sampling && pair_index > random_sample_bound) {
        pair_index--;
        break;
      }
    }

    std::cout << std::endl << "   Done" << std::endl;
    std::cout << "   " << ret.size() << " sequences found, " << pair_index << " examined" << std::endl;
    if (print_to_file) {
      output_file.close();
    }

    return ret;
  } else {
    throw;
  }
}

int main(int argc, char **argv) {
  srand(time(NULL));

  int n = std::stoi(argv[1]);
  int m = std::stoi(argv[2]);

  long random_sample_bound = 0;
  if (argc > 4 && std::string(argv[3]) == "-r") {
    random_sample_bound = std::stol(argv[4]);
  }

  bool write_to_file = false;
  if (argc > 5 && std::string(argv[5]) == "-w") {
    write_to_file = true;
  }

  nested_marvellous(n, m, random_sample_bound, write_to_file);
  return 0;
}
