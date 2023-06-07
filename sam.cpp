#include <iostream>
#include <iomanip>
#include <algorithm> // for transform function
#include <string> // for tolower function

using namespace std;

int main() {
    double distance;
    double speed;
    double time;
    int oilLevel;
    string tirePressure;
    string engineTemperature;
    string batteryAlert;
    
    char unit;
    cout << "Is the distance in miles or kilometers? Please Enter 'm' for miles or 'k' for kilometers: ";
    cin >> unit;

    cout << "Please Enter the distance: ";
    cin >> distance;

    // Validate distance
    if (distance <= 0) {
        cout << "Distance must be a positive value." << endl;
        return 0;
    }

    cout << "Please Enter the average speed (in kilometers/minute): ";
    cin >> speed;

    // Validate speed
    if (speed <= 0) {
        cout << "Speed must be a positive value." << endl;
        return 0;
    }

    // Convert distance to kilometers if given in miles
    if (unit == 'm') {
        distance *= 1.6; // Convert miles to kilometers
    }

    // Calculate the time
    time = distance / speed;

    cout << "\n" << endl;

    // Check the safety of the car systems
    cout << "Let me ask you some questions about your car safety:" << endl;

    cout << "1.What is the oil level on the dipstick, enter 0 for below minimum, 1 for between minimum and maximum, or 2 for above maximum?" << endl;
    cin >> oilLevel;

    cout << "2.Is the front and rear tire pressure between 30 and 35? Enter 'Yes' or 'No': ";
    cin >> tirePressure;

    cout << "3.Is the engine temperature warning light ON? Enter 'Yes' or 'No': ";
    cin >> engineTemperature;

    cout << "4.Is the battery alert light ON? Enter 'Yes' or 'No': ";
    cin >> batteryAlert;

    // Convert all answers to lowercase for case-insensitive comparison
    transform(tirePressure.begin(), tirePressure.end(), tirePressure.begin(), ::tolower);
    transform(engineTemperature.begin(), engineTemperature.end(), engineTemperature.begin(), ::tolower);
    transform(batteryAlert.begin(), batteryAlert.end(), batteryAlert.begin(), ::tolower);

    // Check Sam's answers and display safety actions
    if (oilLevel == 1 && tirePressure == "yes" && engineTemperature == "no" && batteryAlert == "no") {
        cout << "\n" << endl;
        cout << fixed << setprecision(3);
        cout << "To travel " << distance << " kilometers from your town to destination, it takes " << time << " minutes at average speed of " << speed << " kilometers/minute" << endl;
        cout << "Have a safe trip! Your car is free of any issues." << endl;
    } else {
        cout << "\n" << endl;
        cout << fixed << setprecision(3);
        cout << "To travel " << distance << " kilometers from your town to destination, it takes " << time << " minutes at average speed of " << speed << " kilometers/minute" << endl;
        cout << "\n" << endl;
        cout << "Please, take the following actions for your safety before traveling to your destination:" << endl;
        cout << "-----------------------------------------------------" << endl;
        cout << setw(40) << left << "Cause" << setw(80) << left << "Action" << endl;
        cout << setw(40) << left << "**********************" << setw(80) << left << "**********************" << endl;


        if (oilLevel == 0 ) {
            cout << setw(40) << left << "Oil Level Below Minimum" << setw(80) << left << "You should add oil as needed and inspect for any potential oil leaks." << endl;
        }
        if (oilLevel == 2) {
            cout << setw(40) << left << "Oil Level Above Maximum" << setw(80) << left << "You should visit a repair shop to drain the excess oil." << endl;
        }

        if (tirePressure == "no"){
            cout << setw(40) << left << "Tires Pressures out of 30-35 range" << setw(80) << left << "Check the tire pressure and adjust it as needed by inflating or deflating the tires." << endl;
        }

        if (engineTemperature == "yes") {
            cout << setw(40) << left << "ETW Light On" << setw(80) << left << "Call a roadside service to tow your car to a repair shop." << endl;
        }

        if (batteryAlert == "yes") {
            cout << setw(40) << left << "Battery Alert" << setw(80) << left << "You should visit a repair shop to have your electrical system checked for faults. A new battery may be necessary." << endl;
        }
    }

    return 0;
}
