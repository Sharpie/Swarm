# (manor) this is used by EZBin in order to show info about contents 
# of bins.
proc active_item_info {graph} {
  global hideOption hideYes hideNo

  $graph marker create text -coords { +Inf +Inf } -name active_info_marker -anchor ne -bg {} $hideOption $hideYes

  uplevel #0 { set foundclosest 0 }
  bind $graph <Shift-ButtonPress-1> {
    if {[%%W element closest %%x %%y closestretval -interpolate 1]} {
      set foundclosest 1
      %%W marker configure active_info_marker -text "$closestretval(x),$closestretval(y)" $hideOption $hideNo
      %%W element activate $closestretval(name) $closestretval(index)
    }
  }
  bind $graph <ButtonRelease-1> {
    if {$foundclosest} {
      %%W marker configure active_info_marker $hideOption $hideYes
      %%W element deactivate $closestretval(name)
      set foundclosest 0
    }
  }
  bind $graph <KeyPress-o> {
    %%W marker configure active_outlier_marker $hideOption $hideNo
  }
  bind $graph <KeyRelease-o> {
    %%W marker configure active_outlier_marker $hideOption $hideYes
  }
  bind $graph <Enter> {
    focus %%W
  }
}

