#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
using namespace std;

int findMax(vector<int> amounts)
{
    int max = 0;
    for (int i = 0; i < amounts.size(); i++)
    {
        if (amounts[i] > max)
        {
            max = amounts[i];
        }
    }
    return max;
}

int main()
{
    ifstream inputFile("day-one-input.txt");
    if (!inputFile.is_open())
    {
        cout << "Unable to open input file.";
        return 1;
    }
    string line;
    int calorieCount = 0;
    vector<int> calorieCounts;

    while (getline(inputFile, line))
    {
        if (line == "")
        {
            calorieCounts.push_back(calorieCount);
            calorieCount = 0;
            continue;
        }
        int currentAmount = stoi(line);
        calorieCount += currentAmount;
    }
    // if the last line of the file is a number and not empty text.
    if (calorieCount != 0)
    {
        calorieCounts.push_back(calorieCount);
        calorieCount = 0;
    }
    inputFile.close();

    int max = findMax(calorieCounts);
    cout << max << endl;
    return 0;
}