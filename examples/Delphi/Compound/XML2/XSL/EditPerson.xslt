<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<xsl:template match="CompanyName">
		<xsl:element name="a">
			<xsl:attribute name="href">#</xsl:attribute>
			<xsl:attribute name="onclick">DisplayObject("<xsl:value-of select="..//@XML.boldID"/>");</xsl:attribute>
			<span style="color:maroon; cursor:hand; font-size:larger">
				<xsl:apply-templates/>
			</span>			
		</xsl:element>
	</xsl:template>
	
	<xsl:template match="Employment">
		<xsl:apply-templates/>	
	</xsl:template>

	<xsl:template match="FirstName">
		<xsl:text>FirstName</xsl:text>
		<br/>
		<xsl:element name="input">
			<xsl:attribute name="type">text</xsl:attribute>
			<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
			<xsl:attribute name="value"><xsl:value-of select="."/></xsl:attribute>
		</xsl:element>
	</xsl:template>

	<xsl:template match="Company">
		<xsl:for-each select="CompanyName">
			<xsl:apply-templates/>
		</xsl:for-each>
	</xsl:template>

	<xsl:template match="LastName">
		<xsl:text>LastName</xsl:text>
		<br/>
		<xsl:element name="input">
			<xsl:attribute name="type">text</xsl:attribute>
			<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
			<xsl:attribute name="value"><xsl:value-of select="."/></xsl:attribute>
		</xsl:element>
	</xsl:template>

	<xsl:template match="Person">
		<xsl:apply-templates select="FirstName"/>
		<br/>
		<xsl:apply-templates select="LastName"/>
		<br/>
		<xsl:for-each select="Employer">
			<br/>
			<xsl:text>Employer:     </xsl:text>
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
								
		<xsl:for-each select="Employment">
			<xsl:for-each select="Employment">
				<xsl:variable name="vEmp" select="./@XML.idref"/>
				<xsl:for-each select="../../../Employment[@XML.boldID=$vEmp]">									
						<xsl:for-each select="Title">
						<span style="font-size:medium">
							<xsl:text>Title</xsl:text>
						</span>	
						<br/>
						<xsl:element name="input">
							<xsl:attribute name="type">text</xsl:attribute>
							<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
							<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
						</xsl:element>
						<br/>
					</xsl:for-each>
					<xsl:for-each select="PhoneExt">
						<span style="font-size:medium">
							<xsl:text>PhoneExt</xsl:text>
						</span>	
						<br/>
						<xsl:element name="input">
							<xsl:attribute name="type">text</xsl:attribute>
							<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
							<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
						</xsl:element>
					</xsl:for-each>
					<br/>
					<xsl:for-each select="Email">
						<span style="font-size:medium">
							<xsl:text>Email</xsl:text>
						</span>	
						<br/>
						<xsl:element name="input">
							<xsl:attribute name="type">text</xsl:attribute>
							<xsl:attribute name="size">25</xsl:attribute>
							<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
							<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
						</xsl:element>
					</xsl:for-each>
					<br/>
					<br/>
				</xsl:for-each>									
			</xsl:for-each>
		</xsl:for-each>
		<xsl:element name="input">
			<xsl:attribute name="type">submit</xsl:attribute>
			<xsl:attribute name="Value">Save</xsl:attribute>
			<xsl:attribute name="name">btnSubmit</xsl:attribute>
		</xsl:element>
		<br/>
		<br/>
		<br/>	
	</xsl:template>
	
	<xsl:template match="/">
		<html>
			<head>
				<script language="javascript">
					function DisplayObject(BoldId)
					{
					}
				
				</script>
			</head>
			<body>
				<xsl:for-each select="OrgChartClasses">
					<form method="Post" name="frmMain" action="">
						<xsl:element name="input">
							<xsl:attribute name="type">hidden</xsl:attribute>
							<xsl:attribute name="name">Action</xsl:attribute>
							<xsl:attribute name="value">Update</xsl:attribute>
						</xsl:element>												
						<span style="font-size:large">Edit Person</span> <br/>
						<br/>
						<xsl:for-each select="Person">
							<xsl:apply-templates select="."/>
						</xsl:for-each>
						<br/>
					</form>						
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
