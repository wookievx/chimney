package io.scalaland.chimney.example;

import java.util.List;

public class NestedBean implements java.io.Serializable{  
  private List<Integer> ids;  
  private JavaBean nested;  
  public NestedBean(){}  
  public void setIds(List<Integer> ids){this.ids=ids;}  
  public List<Integer> getIds(){return ids;}  
  public void setNested(JavaBean nested){this.nested=nested;}  
  public JavaBean getNested(){return nested;}  
}  