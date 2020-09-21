<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<xsl:template match="@XML.boldID">
		<xsl:element name="input">
			<xsl:attribute name="type">hidden</xsl:attribute>
			<xsl:attribute name="name">TBoldID</xsl:attribute>
			<xsl:attribute name="value"><xsl:value-of select="."/></xsl:attribute>
		</xsl:element>
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="Departments">
		<span style="font-weight:bold">Departments:   </span>
		<xsl:for-each select="../../Department[@XML.boldID=..//@XML.idref]">
			<br/>
			<xsl:for-each select="Name">
				<span style="cursor:hand">
					<xsl:element name="a">
						<xsl:attribute name="href">#</xsl:attribute>
						<xsl:attribute name="onclick">EditObject("<xsl:value-of select="..//@XML.boldID"/>");</xsl:attribute>
						<xsl:apply-templates/>
					</xsl:element>
				</span>
				<xsl:apply-templates select="@XML.boldID"/>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template match="address">
		<span style="font-weight:bold">Address:   </span>
		<xsl:for-each select="../../Address[@XML.boldID=..//@XML.idref]">
			<br/>
			<xsl:for-each select="street">
				<xsl:apply-templates/>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="postcode">
				<xsl:apply-templates/>
  				 <xsl:text>  </xsl:text> 
			</xsl:for-each>			
			<xsl:for-each select="city">
				<xsl:apply-templates/>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="country">
				<xsl:apply-templates/>
			</xsl:for-each>
			<br/>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template match="Company">
		<div>
			<xsl:for-each select="CompanyName">
				<span style="color:maroon; font-size:large">
					<xsl:element name="a">
						<xsl:attribute name="href">#</xsl:attribute>
						<xsl:attribute name="onClick">EditObject("<xsl:value-of select="..//@XML.boldID"/>");</xsl:attribute>
						<xsl:apply-templates/>
					</xsl:element>
				</span>
			</xsl:for-each>
		</div>
		<xsl:for-each select="Offices">
			<div/>
			<xsl:for-each select="Office">
				<xsl:apply-templates select="."/>
			</xsl:for-each>
			<br/>
		</xsl:for-each>
		<br/>
	</xsl:template>
	
	<xsl:template match="Office">
		<div/>
		<br/>
		<xsl:for-each select="../../../Office[@XML.boldID=..//@XML.idref]">
			<div>
				<xsl:for-each select="Name">
					<span style="font-size:medium">
						<xsl:apply-templates/>
					</span>
				</xsl:for-each>
			</div>
			<xsl:for-each select="phone">
				<span style="font-weight:bold">Phone:   </span>
				<xsl:apply-templates/>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="fax">
				<span style="font-weight:bold">Fax:   </span>
				<xsl:apply-templates/>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="Email">
				<span style="font-weight:bold">Email:   </span>
				<xsl:apply-templates/>
			</xsl:for-each>
			<br/>
			<br/>
			<xsl:for-each select="address">
				<xsl:apply-templates select="."/>
			</xsl:for-each>
			<br/>
			<br/>
			<xsl:for-each select="Departments">
				<xsl:apply-templates select="."/>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template match="/">
		<html>
			<head>
				<script language="javascript">
				function EditObject(BoldId)
				{
					window.document.frmCompany.BOLDID.value=BoldId;
					window.document.frmCompany.submit();	
				}
				</script>
			</head>
			<body>
				<xsl:for-each select="OrgChartClasses">
					<span style="font-size:large">Companies</span>
					<br/>
					<br/>				
					<form action="" name="frmCompany" method="post" >
						<xsl:element name="input">
							<xsl:attribute name="type">hidden</xsl:attribute>
							<xsl:attribute name="name">BOLDID</xsl:attribute>
							<xsl:attribute name="value"></xsl:attribute>
						</xsl:element>
						<xsl:element name="input">
							<xsl:attribute name="type">hidden</xsl:attribute>
							<xsl:attribute name="name">Action</xsl:attribute>
							<xsl:attribute name="value">FetchID</xsl:attribute>
						</xsl:element>						
						<xsl:for-each select="Company">
							<xsl:apply-templates select="."/>
						</xsl:for-each>
						<br/>
					</form>	
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
