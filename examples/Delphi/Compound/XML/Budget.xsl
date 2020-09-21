<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">
  <xsl:template match="CELL">
    <TD width="">
      <xsl:element name="input">
         <xsl:attribute name="type">text</xsl:attribute>
         <xsl:attribute name="name"><xsl:value-of select="@BoldID"/></xsl:attribute>
         <xsl:attribute name="value"><xsl:value-of select="."/></xsl:attribute>
      </xsl:element>
    </TD>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="/">
    <HTML>
      <BODY>
        <form method="POST">      
        <H1>Budget <xsl:value-of select="BUDGET/@aName"/> <xsl:value-of select="BUDGET/@aNumber"/></H1>
          <TABLE border="1" width="">
            <TR>
              <TD></TD>
              <xsl:for-each select="BUDGET/COLUMNS/COL">
                <TD><xsl:value-of select="@aName"/></TD>
              </xsl:for-each>
            </TR>
            <xsl:for-each select="BUDGET/ROWS/ROW">
              <TR>
                <TD><xsl:value-of select="@aName"/></TD>
                <xsl:apply-templates select="CELL"/>
              </TR>
            </xsl:for-each>
          </TABLE>
          <input type="submit" value="Update changes" name="BUTTON_UPDATE"/>          
        </form>
      </BODY>
    </HTML>
  </xsl:template>
</xsl:stylesheet>