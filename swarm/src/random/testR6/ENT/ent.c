/*
        ENT  --  Entropy calculation and analysis of putative
                 random sequences.

        Designed and implemented by John "Random" Walker in May 1985.
        Multiple analyses of random sequences added in December 1985.

*/

#include <stdio.h>
#include <ctype.h>
#include <math.h>

#define FALSE 0
#define TRUE  1

#ifdef M_PI
#define PI       M_PI
#else
#define PI       3.14159265358979323846
#endif
#define log2of10 3.32192809488736234787

#define V (void)

static int counts = FALSE;         /* Print character counts */
static int fold = FALSE;           /* Fold upper to lower */
static FILE *fp;                   /* Input file pointer */
static int gotfile = FALSE;        /* Obtained file flag */

static long ccount[256] = {0},     /* Bins to count occurrences of values */
        totalc = 0;                /* Total bytes counted */
static double prob[256];           /* Probabilities per bin for entropy */

/*  Table of chi-square Xp values versus corresponding probabilities  */

static double chsqt[2][10] = {
        0.5,
        0.25,
        0.1,
        0.05,
        0.025,
        0.01,
        0.005,
        0.001,
        0.0005,
        0.0001,

        0.0,
        0.6745,
        1.2816,
        1.6449,
        1.9600,
        2.3263,
        2.5758,
        3.0902,
        3.2905,
        3.7190

};

/*  LOG2  --  Calculate log to the base 2  */

static double log2(x)
  double x;
{
        return log2of10 * log10(x);
}

/*  HELP  --  Print information on how to call  */

static void help()
{
        V printf("\nENT --  Calculate entropy of file.  Call");
        V printf("\n        with ent [options] input-file");
        V printf("\n");
        V printf("\n        Options:   -C   Print occurrence counts");
        V printf("\n                   -F   Fold upper to lower\n");
}

int main(argc, argv)
  int argc;
  char *argv[];
{
        int i, opt, mp, sccfirst, sccdef;
        char *cp;
        register int c;
        unsigned int monte[4];
        long inmont, mcount;
        double a, cexp, incirc, montex, montey, montepi, chip,
               scc, sccun, sccu0, scclast, scct1, scct2, scct3,
               ent, chisq, datasum;

        for (i = 1; i < argc; i++) {
           cp = argv[i];
           if (*cp == '-') {
              opt = *(++cp);
              if (islower(opt)) {
                 opt = toupper(opt);
              }

              switch (opt) {

                 case 'C':
                    counts = TRUE;
                    break;

                 case 'F':
                    fold = TRUE;
                    break;

                 case '?':
                    help();
                    return 0;
              }
           } else {
              if (gotfile) {
                 V printf("Duplicate file name.\n");
                }
              if ((fp = fopen(cp, "rb")) != NULL) {
                 gotfile = TRUE;
              } else {
                 V printf("Cannot open file %s\n", cp);
                 return 2;
              }
           }
        }
        if (!gotfile) {
           help();
           return 2;
        }

        /* Initialise for calculations */

        ent = 0.0;                 /* Clear entropy accumulator */
        chisq = 0.0;               /* Clear Chi-Square */
        datasum = 0.0;             /* Clear sum of bytes for arithmetic mean */

        mp = 0;                    /* Reset Monte Carlo accumulator pointer */
        mcount = 0;                /* Clear Monte Carlo tries */
        inmont = 0;                /* Clear Monte Carlo inside count */
        incirc = 65535.0 * 65535.0;/* In-circle distance for Monte Carlo */

        sccfirst = sccdef = TRUE;  /* Mark first time for serial correlation */
        scct1 = scct2 = scct3 = 0.0; /* Clear serial correlation terms */

        /* Scan input file and count character occurrences */

        while ((c = fgetc(fp)) != EOF) {
           if (fold && isalpha(c) && isupper(c)) {
              c = tolower(c);
           }
           ccount[c]++;            /* Update counter for this bin */
           totalc++;

           /* Update inside / outside circle counts for Monte Carlo
              computation of PI */

           monte[mp++] = c;        /* Save character for Monte Carlo */
           if (mp >= 4) {          /* Calculate every fourth character */
              mp = 0;
              mcount++;
              montex = (monte[0] << 8) | monte[1];
              montey = (monte[2] << 8) | monte[3];
              if ((montex * montex + montey *  montey) <= incirc) {
                 inmont++;
              }
           }

           /* Update calculation of serial correlation coefficient */

           sccun = c;
           if (sccfirst) {
              sccfirst = FALSE;
              scclast = 0;         /* Get rid of High C diagnostic */
              sccu0 = sccun;
           } else {
              scct1 = scct1 + scclast * sccun;
           }
           scct2 = scct2 + sccun;
           scct3 = scct3 + (sccun * sccun);
           scclast = sccun;

        }
        V fclose(fp);

        /* Complete calculation of serial correlation coefficient */

        scct1 = scct1 + scclast * sccu0;
        scct2 = scct2 * scct2;
        scc = totalc * scct3 - scct2;
        if (scc == 0.0) {
           sccdef = FALSE;
        } else {
           scc = (totalc * scct1 - scct2) / scc;
        }

        /* Scan bins and calculate probability for each bin and
           Chi-Square distribution */

        cexp = totalc / 256.0;     /* Expected count per bin */
        for (i = 0; i < 256; i++) {
           prob[i] = (double) ccount[i] / totalc;
           a = ccount[i] - cexp;
           chisq = chisq + (a * a) / cexp;
           datasum += ((double) i) * ccount[i];
        }

        /* Print bin counts if requested */

        if (counts) {
           V printf("Ascii Char Occurrences Probability\n\n");
           for (i = 0; i < 256; i++) {
              if (ccount[i]) {
                 V printf("%3d   %c   %10ld   %f\n", i, i < 32 ? ' ' : i,
                    ccount[i], prob[i]);
              }
           }
           V printf("\nTotal:    %10ld   %f\n\n", totalc, 1.0);
        }

        /* Calculate entropy */

        for (i = 0; i < 256; i++) {
           if (prob[i] > 0.0) {
              ent += prob[i] * log2(1 / prob[i]);
           }
        }

        /* Calculate Monte Carlo value for PI from percentage of hits
           within the circle */

        montepi = 4.0 * (((double) inmont) / mcount);

        /* Calculate probability of observed distribution occurring from
           the results of the Chi-Square test */

        chip = sqrt(2.0 * chisq) - sqrt(2.0 * 255.0 - 1.0);
        a = fabs(chip);
        for (i = 9; i >= 0; i--) {
           if (chsqt[1][i] < a) {
              break;
           }
        }
        chip = (chip >= 0.0) ? chsqt[0][i] : 1.0 - chsqt[0][i];


        /* Print calculated results */

        V printf("Entropy = %f bits per character.\n", ent);
        V printf("\nOptimum compression would reduce the size\n");
        V printf("of this %ld character file by %d percent.\n\n", totalc,
                 (short) ((100 * (8 - ent) / 8.0)));
        V printf(
           "Chi square distribution for %ld samples is %1.2f, and randomly\n",
           totalc, chisq);
        V printf("would exceed this value %1.2f percent of the times.\n\n",
           chip * 100);
        V printf(
           "Arithmetic mean value of data bytes is %1.2f (128 = random).\n",
           datasum / totalc);
        V printf("Monte Carlo value for PI is %1.9f (error %1.2f percent).\n",
           montepi, 100.0 * (fabs(PI - montepi) / PI));
        V printf("Serial correlation coefficient is ");
        if (sccdef) {
           V printf("%1.6f (totally uncorrelated = 0.0).\n", scc);
        } else {
           V printf("undefined (all values equal!).\n");
        }
        return 0;
}
