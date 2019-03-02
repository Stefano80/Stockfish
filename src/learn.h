class Learn
{
protected:
  unsigned int stimct;
  unsigned int resct;
  float threshold;
  float **weights;
  float *result;
  float *_stim;  // place to put stim + bias
  void set_stim(float *s);
public:
  Learn(unsigned int stimulusct, unsigned int resultct);
  ~Learn();
  int init(void);
  int train(float *stim, unsigned int result, unsigned int prediction, float wt=1.0f);
  unsigned int infer(float *stim);
  void setThreshold(float t) { threshold = t; };
  float getThreshold(void) { return threshold; };
  void dump(void);
};