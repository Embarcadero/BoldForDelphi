<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<xsl:template match="CompanyName">
		<xsl:element name="a">
			<xsl:attribute name="href">#</xsl:attribute>
			<xsl:attribute name="onclick">EditObject("<xsl:value-of select="..//@XML.boldID"/>");</xsl:attribute>
			<span style="color:maroon; cursor:hand; font-size:larger">
				<xsl:apply-templates/>
			</span>			
		</xsl:element>
	</xsl:template>

	<xsl:template match="FirstName">
		Name: 
		<xsl:element name="a">
			<xsl:attribute name="href">#</xsl:attribute>
			<xsl:attribute name="onclick">EditObject("<xsl:value-of select="..//@XML.boldID"/>");</xsl:attribute>
			<span style="cursor:hand; font-size:larger">
				<xsl:value-of select="../LastName"/>
				<xsl:text> </xsl:text>
				<xsl:apply-templates/>				
			</span>
		</xsl:element>
	</xsl:template>

	<xsl:template match="Company">
		<xsl:for-each select="CompanyName">
			<xsl:apply-templates/>
		</xsl:for-each>
	</xsl:template>

	<xsl:template match="/">
		<html>
			<head>
				<script language="javascript">
					function EditObject(BoldId)
					{						
						window.document.frmMain.BOLDID.value=BoldId
						window.document.frmMain.submit();						
					}					 
				</script>
			</head>
			<body>				
				<xsl:for-each select="OrgChartClasses">
					<span style="font-size:large">Persons</span> <br/>
					<br/>
					<form  name="frmMain" method="post" action="" >
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
					<xsl:for-each select="Person">
						<xsl:apply-templates select="FirstName"/>
						<br/>
						<xsl:for-each select="Employer">Employer: 
							<xsl:for-each select="Company">
								<xsl:for-each select="../../../Company[@XML.boldID=..//@XML.idref]">
									<xsl:for-each select="CompanyName">
										<xsl:apply-templates select="."/>
									</xsl:for-each>
								</xsl:for-each>
							</xsl:for-each>
							<br/>
						</xsl:for-each>
						<br/>
					</xsl:for-each>
					</form>
					<br/>
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
