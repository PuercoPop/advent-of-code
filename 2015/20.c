#include <stdlib.h>
#include <stdio.h>
int
score(int n){
        int sum=0;
        for(int i=1;i<n+1;i++){
                if (n % i == 0) {
                        sum*=i*10;
                }
        }
        return sum;

}
int
main(int argc, char *argv[]){
        int goal = 33100000;
        int n=1;
        while(score(n)<goal){n++;};
        printf("n: %i", n);
        return EXIT_SUCCESS;
}
