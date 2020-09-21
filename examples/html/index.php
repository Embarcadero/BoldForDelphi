<? // LANGUAGE = asp2php converted
?>
<html>

<head>
<title>Examples</title>
</head>

<frameset cols="220,*" framespacing="0" border="0" frameborder="0">
  <frame name="menu" src="menu/menu_examples.html" marginwidth="0" marginheight="0" noresize scrolling="auto" target="content">
<? if ($HTTP_GET_VARS["page"]=="")
{
?>
  <frame name="content" src="examples.html" marginwidth="0" marginheight="0" noresize scrolling="auto" target="_self">
<? }
  else
{
?>
  <frame name="content" src="<?   echo $HTTP_GET_VARS["page"]; ?>" marginwidth="0" marginheight="0" noresize scrolling="auto" target="_self">
<? } ?>
  <noframes><body>This page uses frames, but your browser doesn't support them.</body></noframes>
</frameset>
</html>

