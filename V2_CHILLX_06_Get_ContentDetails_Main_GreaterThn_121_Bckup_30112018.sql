



--EXEC [V2_CHILLX_06_Get_ContentDetails_Main_GreaterThn_121] 'English','ChillXPremium',5770044,'51762','MOBILE_4G','21','122','1466069437402','','Test',0
CREATE  PROCEDURE [dbo].[V2_CHILLX_06_Get_ContentDetails_Main_GreaterThn_121]
(
	 @Lang			VARCHAR(100), 
	 @MasterKey		VARCHAR(100), --Security Key
	 @UserID		VARCHAR(100), 
	 @PContentID	VARCHAR(100),
	 @Network		VARCHAR(200) = '%%',
	 @NetSpeed		VARCHAR(200) = '%%',
	 @VersionCode	VARCHAR(200) = '%%',
	 @TS			VARCHAR(200) = '%%',
	 @REGID			Varchar(20)  = '',
	 @DisplayID		Varchar(300) = '',
	 @UGC_Content   Varchar(2)	 = 0,
	 @StoredLink    VARCHAR(8000)='',
	 @OrderId       VARCHAR(100) ='',
	 @IsPremium     INT		 = 0,
	 @DistributionMasterKey VARCHAR(8000)='',
	 @Series_Name	VARCHAR(500) =  '',
	 @Season_Name	VARCHAR(500) =  ''  
)
AS
BEGIN

	SET NOCOUNT ON;

	Select @IsPremium=ISNULL(ISPremium,0) 
	From Atom_Content_Approved 
	Where Content_Id=@PContentID

	Declare @Bought int = 1; -- If not Purchased then Bought =1 , But if you buy then Bought=0 (Ulta Logic here)
	Declare @IsHD int = 0;

 ----------Added to check PPD Status--------  

	DECLARE
	@Series_Nm			VARCHAR(1000),
	@Series_Id			INT,
	@Season_Id			INT,
	@subscriber			INT=0,
	@LONG_DESCRIPTION	NVARCHAR(1000),
	@Allow_To_Play		INT,
	@Refund_Eligible	INT,
	@SubTitle			NVARCHAR(4000)

	Select @Series_Id=Series_Id,@Season_Id=Season_Id
	From V2_BaseMaster
	Where PContentId=@PContentID And UI_Lang=@Lang

	Select PContentID
	Into #Content
	From V2_BaseMaster
	Where Series_Id=@Series_Id And Season_Id=@Season_Id And UI_Lang=@Lang

	Select @subscriber = 1,
		   @Refund_Eligible= Refund_Eligible
	From ATOM_usertxns U
	JOIN #Content C
		ON C.PContentID=U.CP_Id
	Where [user_id] IN (Select [user_id] from atom_userdetails_x where regid = @REGID)
		  AND isSuccess = 1 AND purchtype In ('PPD','Paytm')

    ----------------------Added on 17 Nov18 tO Show series_description---------

    Select @Series_Id=Series_Id
	From Atom_Content_Approved
	Where Content_Id=@PContentID  

    Select @LONG_DESCRIPTION= CASE WHEN @Lang='English' THEN ISNULL(Series_Description,'')
	                               ELSE ISNULL(Series_Description_Hin,'')
                              END,
		   @SubTitle		= CASE WHEN @Lang='English' THEN ISNULL(Series_Short_Descr,'')
	                               ELSE ISNULL(Series_Short_Descr_Hin,Series_Short_Descr)
                              END
	From Tbl_Series_Master
	Where Id=@Series_Id


	--Select Content_Id,LONG_DESCRIPTION
	--Into #Season_Episode_Name
	--From atom_content_approved
	--Where Episode_No=1 And Content_Id=@PContentID

    ----------------------Limited same User_Id play to video-------------------------------

	select  COUNT(DISTINCT USER_ID) Cnt_UserId
	Into #Watched_User 
	From V2_ATOM_USERACTIONS
	Where ActionType='Watched' 
		  AND ActionDate>=CONVERT(DATE,GETDATE()) AND ActionDate<DATEADD(D,1,CONVERT(DATE,GETDATE()))
		  AND RegId=@REGID

	--Select @Allow_To_Play=CASE WHEN Cnt_UserId<=5 THEN 1
	--						   ELSE 0
	--					  END
	--From #Watched_User


	SET @Allow_To_Play=1

	--IF @IsPremium=1 And ISNULL(@UGC_Content,0)=0
	IF 1=2
	BEGIN

		--Print '1'
		Select Lang,			PContentID,			GenreID,		 GenreName,			ParentID,	PCTitle,		
				PCSubtitle1,		PCSubtitle2,		PCDetails,		 
				IsSaranyu= CASE WHEN @VersionCode>121 And IsSaranyu=6 THEN 7
								ELSE IsSaranyu
							END,
				PCRating,	
				PlayURL= CASE WHEN @VersionCode>121 And IsSaranyu=6 THEN BINARYFILE_URL
							  ELSE PlayURL
						 END,	
				ContentImageTag, ContentImageTagName,
				ShowRatingsFlag, PackageName,		VersionCode,	 VersionName,		NoOfLikes,	NoOfSaves,		
				NoOfComments,	Poster_1URL,		Poster_2URL,	 Poster_3URL,		Poster_4URL,PreImage1,	
				PreImage2,		PreImage3,			PreImage4,		 PreImage5,			
				--IsDownloadable= CASE WHEN IsSaranyu In (0,1,6) THEN 1
				--					 ELSE 0
				--				END,	 
				IsDownloadable= CASE WHEN IsSaranyu In (0) THEN 1
									 ELSE 0
								END,
				ShowDetailsSection,ShowImagesSection,	
				TouchfoneSize240,TouchfoneSize360,	TouchfoneSize480,TouchfoneSize720,	TouchfoneURL240,
				TouchfoneURL360,	TouchfoneURL480,	TouchfoneURL720, PlayType,		ISNULL(APKSize,0) [APKSize],	
				PCLiked,	
				PCSaved,			PCWatched,			PCResume,		 PCRemark,			PCSavedRemark,	
				PCPurchased=1,	
				BuyFlag  = CASE WHEN @Bought	 = 0 THEN 0
								WHEN @ISPREMIUM  = 1 THEN 1
								WHEN @subscriber = 1 THEN 0
								ELSE BuyFlag
						  END,
				Subcriber=@subscriber,		 
				PCPrice=CASE WHEN ISNULL(Is_Series,0)=1 THEN 10
							 ELSE PCPrice
						END,			
				ShowSubscription,	
				SubScription_Heading,SubScription_Cost,SubScription_Text,
				SDRate1=CASE WHEN ISNULL(Is_Series,0)=1 THEN 10
							 ELSE SDRate1
						END,			
				SDPeriod1,		SDRate2,	
				SDPeriod2,		HDRate1,			HDPeriod1,		HDRate2,		HDPeriod2,	 SDText1='Buy Now',	
				SDText2='Buy Now',		HDText1,			HDText2,		SubcribeDay,	SubcribeWeek,SubcribeYear,	
				Isfortumo=1,		
				IsHD,			PurchaseText,		VideoADFlag,	VideoAdSpot,	StatialADFlag,	
				StatialADSpot,	TPAFlag,			TPA_AdSpot,		Ad_StartFlag,	isnull(BannerAd_Flag,0) as BannerAd_Flag,
				isnull(Banner_Ad_Spot,'') as Banner_Ad_Spot,
				BannerAdFlag,	Pre_VideoURL,		UGC_Content,	TagName,		TagURL,
				OrderId=1000,
				ISNULL(INKA_CID,'')			[INKA_CID],
				ISNULL(INKA_CONTENT_NAME,'') [INKA_CONTENT_NAME],
				ISNULL(Series_Name,'') [Series_Name],
				ISNULL(Season_Name,'') [Season_Name],
				ISNULL(Episode_No,0)   [Episode_No],
				ISNULL(Is_Series,0)    [Is_Series], 
				ISNULL(Is_Season,0)    [Is_Season],
				CASE WHEN Is_Series=1 THEN 1
			         ELSE 0
                END			           [Display_Type],
				'Share & Earn'		   [No_Of_Shares]
		From Tbl_Static_ContentDetails_Main
		Where PContentID=@PContentID AND UI_Lang=@Lang

	END
	ELSE
	BEGIN
		
		--Print '2'			
		IF(@UGC_Content = 1 or @PContentID > 500000)
		BEGIN 

			Exec  [45.113.189.23].Futage.dbo.[Fu_Proc_ContentDetails_Main]  @Lang,@MasterKey,@UserID,@PContentID,@Network,@NetSpeed,@VersionCode,@TS,@REGID,@DisplayID 
/*					
			Insert Into V2_ATOM_USERACTIONS_APILOGS
			(USER_ID,PCONTENT_ID,ACTIONTYPE,[ACTION],ACTIONDATE,REMARK,MasterKey,City,Region,Country,Countrycode,Zipcode,isp,Latitude,
				Longitude,REGID,Decrypted_Key,Network,Lang,VersionCode,TS,Is_UGC)
			Select @UserID,@PContentID,'Viewed',1, GETDATE(),'',@MasterKey,'','','', '', '', '','', '',
					@RegId,@SysMasterKey,@Network,@Lang,@VersionCode,DATEDIFF(s,@TimeStarted,Getdate()),1
*/
		END 
		
		ELSE 
		BEGIN 

			Declare @PurchaseDate datetime;
			Set @PurchaseDate = 'Dec 01 2015';

			IF EXISTS (Select CP_ID from ATOM_PPDUSERSCONTENT where regid = @regid 
						and CP_ID = @PContentID 
						and getdate()<paidenddatetime
						)
			BEGIN
				Set @Bought = 0;
				Set @PurchaseDate = (select max(PaidStartDatetime) from ATOM_PPDUSERSCONTENT where regid = @regid 
						and CP_ID = @PContentID);
			END

			IF EXISTS (Select cp_id from ATOM_usertxns where 
									[user_id] in (Select [user_id] from atom_userdetails_x where regid = @REGID)
									and CP_ID = @PContentID 
									and isSuccess = 1
									and day(TXNSTARTDATE) = day(getdate())
									and month(TXNSTARTDATE) = month(getdate())
									and year(TXNSTARTDATE) = year(getdate())
									)
			BEGIN
				Set @PurchaseDate = (select max(TXNSTARTDATE) from ATOM_usertxns where 
									[user_id] in (Select [user_id] from atom_userdetails_x where regid = @REGID)
									and CP_ID = @PContentID and isSuccess = 1
									and day(TXNSTARTDATE) = day(getdate())
									and month(TXNSTARTDATE) = month(getdate())
									and year(TXNSTARTDATE) = year(getdate()) );
				Set @Bought = 0;
				Set @IsHD = Case when (select max(VideoType) from ATOM_usertxns where 
										[user_id] in (Select [user_id] from atom_userdetails_x where regid = @REGID)
										and CP_ID = @PContentID and isSuccess = 1
										and TXNstartDATE = @PurchaseDate) = 'HD' then 1 else 0 end;

 

				IF @IsHD is null	
					Set @IsHD =0;
			END

			IF EXISTS (Select regid from ATOM_SUBSCRIBEDUSERS where regid = @regid 
															and SUB_ENDDATETIME>=GETDATE())
			BEGIN
				Set @subscriber = 1;

				Select @OrderId=Order_Id 
				From ATOM_SUBSCRIBEDUSERS 
				Where regid = @regid AND SUB_ENDDATETIME>=GETDATE()
			END

			
			Select 
			-----------------------Main Data Info---------------------------------------------------------------
			Lang						= @Lang, 
			A.PContentID,
			GenreID						= A.GENRE_ID,
			GenreName = A.Genre_Name, A.ParentID, A.PCTitle, 
			PCSubtitle1				    = ISNULL(A.PCSubtitle,''), 
			PCSubtitle2					= ISNULL(A.PCSubtitle2,''),  
			PCDetails					= cast(Case When ZZ.Category_ID = 1 then Replace(concat('Cast: ',ZZ.Actors,', ',ZZ.ACTRESSES+ ' '),'#',', ') + CHAR(10) + 
							  														Replace(concat('Director: ',ISNULL(ZZ.DIRECTORS,'NA')),'#',', ')
														Else Replace(ZZ.SHORT_DESCRIPTION,'#',', ')
														End							  
											as nvarchar(max)),
			IsSaranyu= CASE WHEN @VersionCode>121 And A.IsSaranyu=6 THEN 7
							ELSE A.IsSaranyu
						END,
			PCRating=ISNULL(A.PCRating,0.00),
			PlayURL= CASE WHEN @VersionCode>121 And A.IsSaranyu=6 THEN A.BINARYFILE_URL
							ELSE A.PlayURL
						END,
			A.ContentImageTag , A.ContentImageTagName,	A.ShowRatingsFlag,
			A.PackageName, A.VersionCode, A.VersionName,
			NoOfLikes	 = A.PC_NoofLikes,
			NoOfSaves	 = A.PC_NoofSaves,
			NoOfComments = A.PC_NoofComments,
			Poster_1URL  = concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_400x233')),
			Poster_2URL  = concat(@StoredLink,ImagePreview_Text),
			Poster_3URL  = concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_150x150')),
			Poster_4URL  = concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_150x150')),

			PreImage1   =  Replace(concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_1_400x400')), '/images/','/Preview/'),
			PreImage2   =  Replace(concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_2_400x400')), '/images/','/Preview/'),
			PreImage3   =  Replace(concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_3_400x400')), '/images/','/Preview/'),
			PreImage4   =  Replace(concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_4_400x400')), '/images/','/Preview/'),
			PreImage5   =  Replace(concat(@StoredLink,REPLACE(ImagePreview_Text,'_200x233','_5_400x400')), '/images/','/Preview/'),

			IsDownloadable = 	isnull(case when A.smarturlprovider = 4 then 0 
								when ZZ.ISPremium = 1 and ZZ.IsDownloadable = 1 then 1
								WHEN @Bought = 0 THEN 1
								when A.IsPREMIUM = 0 then 1 
								when A.ParentID = 1 then 0 
								when A.isSaranyu  = 6 then 0 
								else ZZ.IsDownloadable end,0),       --?????????????????????????????????????????------
			ShowDetailsSection = isnull(ZZ.descriptionexists,0) ,
			ShowImagesSection = isnull(ZZ.PREVIEWEXISTS,0),
			TouchfoneSize240 = ZZ.FileSize240,
			TouchfoneSize360 = ZZ.FileSize360,
			TouchfoneSize480 = ZZ.FileSize480,
			TouchfoneSize720 = ZZ.File_Size,
			TouchfoneURL240 = ZZ.SMARTURL2SIZE240,
			TouchfoneURL360 = ZZ.SMARTURL2SIZE360,
			TouchfoneURL480 = ZZ.SMARTURL2SIZE480,
			TouchfoneURL720 = ZZ.SMARTURL2SIZE720,
			PlayType		= Case When (A.ParentID=7 OR A.ParentID in (2,1,14)) then 1
									Else 2 End,------ Check why this exists????????????????????????????????----------------
			APKSize  		= Case When ZZ.category_id in (3,4) then ZZ.file_size  
									Else 0 end, ------ Check why this exists????????????????????????????????----------------
			--TouchfoneDownloadUrl = 'A',		
			--Pre_VideoURL ='A',
			--------------------User info-----------------------------------------------------------
			PCLiked = 0, PCSaved = 0, PCWatched = 0,
			PCResume = 0,
			PCRemark = '',
			PCSavedRemark = '',
			PCPurchased = Case when @Bought =0 then 0 else 1 end, 
		
			---------------------------Business Logic------------------------------------------------------
				
			BuyFlag                                  = CASE WHEN @Bought = 0 then 0
															WHEN @subscriber = 1 then 0
															WHEN OL.Discount_Perc=100 Then 0
															WHEN (OL.Discount_Perc > 0 and OL.Discount_Perc < 100) Then 0
															WHEN ZZ.ISPREMIUM = 1 then 1 --Paid content show Buy as 1
															WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.BuyFlag
															WHEN (BL.CATEGORY_ID=ZZ.CATEGORY_ID
															and ((ZZ.TP_ID  = BL.TP_ID and BL.TP_ID> 0) or BL.TP_ID= 0)
															and ((ZZ.TP_ID not in (BL.TP_ID_Exclude) and BL.TP_ID_Exclude > 0) or BL.TP_ID_Exclude = 0)
															and (((ZZ.FILE_SIZE < BL.FILE_SIZE_MAX) and (ZZ.FILE_SIZE >= BL.FILE_SIZE_MIN) AND ZZ.category_ID = 3) OR (ZZ.Category_id not in (3)))) 
															Then BL.BuyFlag
															ELSE 0 END,

			Subcriber								 = @subscriber, --Case when A.category_id=3 then 0 else @subscriber end,

			PCPrice                                  = CASE	   WHEN A.smartUrl1 is null and A.smarturl3 is null and A.smarturl2 is null and ZZ.CATEGORY_ID in (1,2,14) then 'Content Not Live'
																WHEN A.smartUrl2 is null and A.smarturl3 is null and ZZ.category_id = 1  then 'Content Not Live'
																WHEN (A.ispremium = 1 and @Bought = 0)  then case when @Lang not in ('English') then (Select T_Word from V2_T_LangWords_ContentDetails where Words = 'Bought' and T_Lang = @Lang) else 'Bought' end
																WHEN OL.Discount_Perc=100 then OL.OFFER_NAME --Free Movies, Free Games
																WHEN @subscriber=1 and ZZ.category_id in (1) and A.ispremium = 1 then case when @Lang not in ('English') then (Select T_Word from V2_T_LangWords_ContentDetails where Words = 'Subscribed' and T_Lang = @Lang) else 'Subscribed' end
																WHEN @subscriber=1 And A.ispremium = 1 And @Bought = 0 And ZZ.category_id not in (3) Then case when @Lang not in ('English') then (Select T_Word from V2_T_LangWords_ContentDetails where Words = 'Subscribed' and T_Lang = @Lang) 
																else 'Subscribed' end
																WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then case when @Lang not in ('English') then (Select T_Word from V2_T_LangWords_ContentDetails where Words = 'Buy' and T_Lang = @Lang) else 'Buy'  end 
																WHEN (BL.CATEGORY_ID=ZZ.CATEGORY_ID
																					and ((ZZ.TP_ID  = BL.TP_ID and BL.TP_ID> 0) or BL.TP_ID= 0)
																					and ((ZZ.TP_ID not in (BL.TP_ID_Exclude) and BL.TP_ID_Exclude > 0) or BL.TP_ID_Exclude = 0)
																					and ((ZZ.FILE_SIZE <= BL.FILE_SIZE_MAX) or ZZ.FILE_SIZE IS NULL)) Then case when @Lang not in ('English') then (Select T_Word from V2_T_LangWords_ContentDetails where Words = 'Buy' and T_Lang = @Lang) else 'Buy' end
																WHEN (ZZ.ispremium = 1 and @Bought = 1 and (OL.Discount_Perc>0 and OL.Discount_Perc<100))
																	THEN case when @Lang not in ('English') then '' else																		  
																	Concat(isnull((Select T_Word from V2_T_LangWords_ContentDetails where Words = 'Discounted Price Starts at Rs.' and T_Lang = @Lang),'Discounted Price Starts at Rs.'),
																	ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDRate1 Else BL.SDRate1 End),0)*Cast(ISNULL((Cast(OL.Discount_Perc as Decimal(10,2))/100),1) as Decimal(10,2)))
																	End
																	--- Discount % betwee 0 & 100
																WHEN (ZZ.ispremium = 1 and @Bought = 1 And (OL.Discount_Perc=0 or OL.Discount_Perc is NULL))
																	Then case when @Lang not in ('English') then '' else																		  
																	Concat(isnull((Select T_Word from V2_T_LangWords_ContentDetails where Words = 'Starting Price Rs' and T_Lang = @Lang),'Starting Price Rs'),
																	ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDRate1 Else BL.SDRate1 End),0)*Cast(Cast(ISNULL((Cast(OL.Discount_Perc as Decimal(10,2))/100),1) as Decimal(10,2)) as Int))
																	End
																-- Other Paid Content
																WHEN ZZ.category_Id in (2,4,14) then '' --Videos, SFs, Apps -- Dont show any Tags
																Else '' END,
			ShowSubscription						 = ISNULL(Case WHEN (A.ISPREMIUM IS NULL OR A.ISPREMIUM =0) then 0
																	Else SL.ShowSubscription End,0),
			SubScription_Heading					 = ISNULL(Case WHEN (A.ISPREMIUM IS NULL OR A.ISPREMIUM =0) then '0'
																	Else SL.SubScription_Heading End,''),
			SubScription_Cost						 = ISNULL(Case WHEN (A.ISPREMIUM IS NULL OR A.ISPREMIUM =0) then 0
																	Else SL.SUB_PRICE End,0),
			SubScription_Text						 = ISNULL(Case WHEN (A.ISPREMIUM IS NULL OR A.ISPREMIUM =0) then '0'
																	Else SL.SubScription_Text End,''),
			SDRate1                                  = 10,
			--SDRate1                                  = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDRate1 Else BL.SDRate1 End),0)*Cast(ISNULL((Cast(OL.Discount_Perc as Decimal(10,2))/100),1) as Decimal(10,2)),
			SDPeriod1								 = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDPeriod1 Else BL.SDPeriod1 End),0),
			SDRate2                                  = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDRate2 Else BL.SDRate2 End),0)*Cast(ISNULL((Cast(OL.Discount_Perc as Decimal(10,2))/100),1) as Decimal(10,2)),
			SDPeriod2								 = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDPeriod2 Else BL.SDPeriod2 End),0),
			HDRate1                      = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.HDRate1 Else BL.HDRate1 End),0)*Cast(ISNULL((Cast(OL.Discount_Perc as Decimal(10,2))/100),1) as Decimal(10,2)),
			HDPeriod1								 = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.HDPeriod1 Else BL.HDPeriod1 End),0),
			HDRate2    = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.HDRate2 Else BL.HDRate2 End),0)*Cast(ISNULL((Cast(OL.Discount_Perc as Decimal(10,2))/100),1) as Decimal(10,2)),
			HDPeriod2								 = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.HDPeriod2 Else BL.HDPeriod2 End),0),
			SDText1    = 'Buy Now',                      
			--SDText1                                  = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDText1 Else BL.SDText1 End),''),
			--SDText2 = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.SDText2 Else BL.SDText2 End),''),
			SDText2    = 'Buy Now',
			HDText1                        = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.HDText1 Else BL.HDText1 End),''),
			HDText2                                  = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.HDText2 Else BL.HDText2 End),''),
			SubcribeDay								 = 55, -- Redundant
			SubcribeWeek							 = 79, --Redundant
			SubcribeYear						     = 249, --Redundant
			Isfortumo								 = 1, --BL.Isfortumo,
			IsHD									 = @IsHD,
			PurchaseText							 = ISNULL((CASE WHEN ZZ.CONTENT_ID = ML.CONTENT_ID then ML.PurchaseText Else BL.PurchaseText End),''),
			----------------------------Ad Logic------------------------------------------------------------
		
			VideoADFlag= Case when EA.AD_TYPE='Interstitial Video' then EA.ADMOB_AD_CODE Else 0 End,
			VideoAdSpot= Case when EA.AD_TYPE='Interstitial Video' then EA.ADMOB_AD_URL Else '0' End,
			StatialADFlag= Case when EA.AD_TYPE='Interstitial Banner' then EA.ADMOB_AD_CODE Else 0 End,
			StatialADSpot= Case when EA.AD_TYPE='Interstitial Banner' then EA.ADMOB_AD_URL Else '0' End,
			TPAFlag= isnull(EA.Ad_Code,0),
			TPA_AdSpot= isnull(EA.AD_URL,'0'),
			Ad_StartFlag= isnull(EA.AD_AT_START,0),
			
			BannerAd_Flag=ISNULL(BA.AD_CODE,0),
			Banner_Ad_Spot=ISNULL(BA.AD_URL,''),
			BannerAdFlag=0,
			
			-------------------------------Dummy Data---------------------------------------------------------
			Pre_VideoURL = '',
			UGC_Content = 0,  
			TagName		= Genre_Name_Display,
			TagURL		= CONCAT('https://s3.ap-south-1.amazonaws.com/funongo/chillx.mobi/CMS_FOG/Preference/',Interest_image_name,'.png'),
			OrderId		= @OrderId,
			ISNULL(ZZ.INKA_CID,'')			[INKA_CID],
			ISNULL(ZZ.INKA_CONTENT_NAME,'') [INKA_CONTENT_NAME],
			CASE WHEN @Lang='ENGLISH' 
				 THEN ISNULL(M.Series_Name,'')
                 ELSE ISNULL(Series_Name_Hin,'')
            END						 [Series_Name],
			ISNULL(A.Season_Name,'') [Season_Name],
			ISNULL(A.Episode_No,0)   [Episode_No],
			ISNULL(A.Is_Series,0)    [Is_Series], 
			ISNULL(A.Is_Season,0)    [Is_Season],
			CASE WHEN A.Is_Series=1 THEN 1
			     ELSE 0
            END			             [Display_Type],
			'Share & Earn'			 [No_Of_Shares],
			Description='',
            Director='',
			Trailer_URL=PlayURL,
            Starring='',
			HeadingParent			 [Category_Name],
			@LONG_DESCRIPTION        [Series_Description],
			@Allow_To_Play           [Allow_To_Play],
			ISNULL(@Refund_Eligible,0) [Refund_Eligible],
			1						 [Is_Distributor],
			@SubTitle				 [Subtitle]  
			from PortalDB..V2_BaseMaster A
			JOIN dbo.Atom_content_Approved ZZ
				ON ZZ.Content_ID = A.PContentID
			LEFT JOIN ATOM_APPGENRE AG
				ON AG.GenreId=A.Genre_Id And AG.UI_Lang=A.UI_Lang
			------------------------------------Language translation --------------------------------------------------------------------------
			--LEFT JOIN T_LangWords_ContentDetails LANGT
			--	on LANGT.Words = ML.PCPrice
			----------------------------------Page Ad Management --------------------------------------------------------------------------
			LEFT JOIN
				(
					Select DistributionKey, SectionID, Ad_Type, Placement_Remarks, Max(AD_Code) Ad_Code, Max(AD_URL) Ad_URL, Max(RecordNo) RecordNo, Max(AD_Name) Ad_Name
					From Portaldb.[dbo].V2_ATOM_ADS_MGMT  AA, Portaldb.dbo.APP_MASTERKEYS BB
					Where AA.DistributionKey in (@DistributionMasterKey,'ALL')
					and AA.Display_Datetime_Start <= GETDATE() and AA.Display_Datetime_Ends >= GETDATE() -- Valid AD_URL in that stipulated period
					and BB.APP_NAME=AA.APKBuild
					and BB.MASTERKEY= @DistributionMasterKey
					and AA.Page_Name = 'ContentPage'
					and AA.Approved_Status = 1
					group By DistributionKey,SectionID, Ad_Type, Placement_Remarks
				)BA on Ba.SectionID = 0
			------------------------------------PLay the content Ad Management --------------------------------------------------------------------------
			LEFT JOIN
				(
						Select
							ADMOB_AD_CODE = case when CC.DIRECT_ADMOB = 1 then CC.AD_CODE else 0 end ,
							ADMOB_AD_URL = case when CC.DIRECT_ADMOB = 1 then CC.AD_URL else '0' end ,
							AD_CODE = case when CC.DIRECT_ADMOB = 0 then CC.AD_CODE else 0 end ,
							AD_URL = case when CC.DIRECT_ADMOB = 0 then CC.AD_URL else '0' end ,
							CC.DIRECT_ADMOB, CC.CATEGORY_ID,CC.MAX_DURATION_Mins,CC.TP_ID_Include,CC.TP_ID_Exclude,CC.IS_PREMIUM,CC.AD_AT_START,CC.AD_TYPE
							From Portaldb.dbo.APP_MASTERKEYS BB, Portaldb.[dbo].[V2_ATOM_ADS_CONTENTPLAY] CC
						where CC.DISTRIBUTION_KEY in (@DistributionMasterKey,'ALL')
						and BB.APP_NAME=CC.APKBuild and BB.MASTERKEY=@DistributionMasterKey and CC.Approved=1
						and CC.PAGE_NAME in ('ContentPlay')
				)EA 
					ON  ZZ.Category_ID=EA.Category_id and (ZZ.ispremium =0 or ZZ.ispremium is NULL)
						And ((A.TP_ID  = EA.TP_ID_Include and EA.TP_ID_Include > 0) or EA.TP_ID_Include = 0)
						And ((A.TP_ID not in (EA.TP_ID_Exclude) and EA.TP_ID_Exclude > 0) or EA.TP_ID_Exclude = 0)
						And ((ZZ.DURATION <= EA.MAX_DURATION_Mins and ZZ.Category_ID in (1,14,7,2)) or ZZ.Category_Id in (3,4) or ZZ.DURATION IS NULL)
			------------------------------------------------------ PPD Pricepoints (Global Strategy) --------------------------------------------------------
			LEFT JOIN
				(      
				Select AA.CATEGORY_ID,AA.[DESC],AA.TP_ID,AA.TP_ID_Exclude,AA.DIST_KEY,AA.FILE_SIZE_MIN,AA.FILE_SIZE_MAX,
										AA.SDRate1, AA.SDPeriod1,AA.SDRate2, AA.SDPeriod2,AA.HDRate1, AA.HDPeriod1,AA.HDRate2, AA.HDPeriod2,
										AA.SDText1,AA.SDText2,AA.HDText1,AA.HDText2,AA.PurchaseText, AA.BUYFLAG, AA.PCPRICE
										from Portaldb.[dbo].[V2_ATOM_PPD_MGMT_STRATEGY] AA, Portaldb.dbo.APP_MASTERKEYS BB
										where AA.Dist_Key in (@DistributionMasterKey,'ALL') and BB.MASTERKEY=@DistributionMasterKey
										AND AA.Approved_Status=1
													--And AA.Category_id > 2
				)BL 
					ON BL.CATEGORY_ID=ZZ.CATEGORY_ID AND (ZZ.ISPREMIUM > 0 OR ZZ.ISPREMIUM IS NULL)
					   And ((ZZ.TP_ID  = BL.TP_ID and BL.TP_ID> 0) or BL.TP_ID= 0)
					   And ((ZZ.TP_ID not in (BL.TP_ID_Exclude) and BL.TP_ID_Exclude > 0) or BL.TP_ID_Exclude = 0)
					   And (((ZZ.FILE_SIZE < BL.FILE_SIZE_MAX) and (ZZ.FILE_SIZE >= BL.FILE_SIZE_MIN) AND ZZ.category_ID = 3) OR (ZZ.Category_id not in (3)))
					   And ZZ.Content_ID not in (Select distinct Content_id from Portaldb.[dbo].[V2_ATOM_PPD_MGMT_MANUAL_CONTENT])
				------------------------------------------------------ PPD Pricepoints (Manually entered) --------------------------------------------------------
				LEFT JOIN
				(          
				Select AA.CONTENT_ID,AA.REMARK,AA.DIST_KEY,
										AA.SDRate1, AA.SDPeriod1,AA.SDRate2, AA.SDPeriod2,AA.HDRate1, AA.HDPeriod1,AA.HDRate2, AA.HDPeriod2,
										AA.SDText1,AA.SDText2,AA.HDText1,AA.HDText2,AA.PurchaseText, AA.BUYFLAG, AA.PCPRICE, AA.ContentTagName, AA.ContentTag
										from Portaldb.[dbo].[V2_ATOM_PPD_MGMT_MANUAL_CONTENT] AA, Portaldb.dbo.APP_MASTERKEYS BB
										where AA.Dist_Key in (@DistributionMasterKey,'ALL') and BB.MASTERKEY=@DistributionMasterKey
										AND AA.Approved_Status=1
										AND AA.StartDateTime <= Getdate() AND AA.EndDateTime > Getdate()
				)ML 
					ON ML.CONTENT_ID=@PContentID
 
				------------------------------------------------------ Offers (Manually entered) --------------------------------------------------------
				LEFT JOIN
				(   
				Select AA.CONTENT_ID,AA.P_OFFER_ID, BB.Discount_Perc, BB.UI_LANG, BB.DISTRIBUTIONKEY, BB.OFFER_NAME, BB.OFFER_DISPLAYNAME
				from Portaldb.[dbo].[ATOM_P_OFFERS_DETAILS] AA, Portaldb.[dbo].[V2_ATOM_P_OFFERS] BB, Portaldb.dbo.APP_MASTERKEYS CC
				WHERE AA.P_OFFER_ID=BB.P_OFFERID
				AND BB.UI_LANG in (@Lang,'All')
				AND BB.DistributionKey in (@DistributionMasterKey,'ALL') and CC.MASTERKEY=@DistributionMasterKey
				AND BB.Approved=1 AND BB.Offer_StartDateTime <= Getdate() AND BB.Offer_EndDateTime> Getdate()
				)OL 
					ON OL.CONTENT_ID=@PContentID
				------------------------------------------------------Subscription Pricepoints --------------------------------------------------------
				LEFT JOIN
				(
				Select AA.[DESC],AA.CATEGORY_ID,AA.GENRE_ID, AA.CONTENT_ID, AA.TP_ID, AA.TP_ID_EXCLUDE,AA.DIST_KEY,
				AA.ShowSubscription,AA.SUB_PRODUCT_ID,AA.SubScription_Heading,AA.SubScription_Text, AA.Approved_Status,
				BB.SUB_PRICE, BB.SUB_DAYS, BB.BILLING_PARTNER
				FROM Portaldb.[dbo].[V2_ATOM_SUB_PRICE_MGMT] AA, Portaldb.[dbo].[V2_ATOM_SUB_PRICE_OPERATORWISE] BB, Portaldb.dbo.APP_MASTERKEYS CC
				where AA.Dist_Key in (@DistributionMasterKey,'ALL') AND CC.MASTERKEY=@DistributionMasterKey AND AA.Approved_Status=1
				AND AA.SUB_PRODUCT_ID=BB.SUB_ID AND BB.CAT_ID=AA.CATEGORY_ID 
				and AA.Row_ID = 1
				--AND BB.Operator=@OperatorName
				)SL
					ON  SL.CATEGORY_ID=ZZ.CATEGORY_ID AND (ZZ.ISPREMIUM > 0 OR ZZ.ISPREMIUM IS NULL)
						And ZZ.GENRE_ID = (Case when SL.GENRE_ID > 0 THEN SL.GENRE_ID ELSE A.GENRE_ID END)
						And ZZ.CONTENT_ID = (Case when SL.CONTENT_ID > 0 THEN SL.CONTENT_ID ELSE ZZ.CONTENT_ID END)
						And ((ZZ.TP_ID  = SL.TP_ID and SL.TP_ID> 0) or SL.TP_ID= 0)
						And ((ZZ.TP_ID not in (SL.TP_ID_Exclude) and SL.TP_ID_Exclude > 0) or BL.TP_ID_Exclude = 0)
				LEFT JOIN Tbl_Series_Master M
					ON M.ID=A.Series_Id
			Where 
			A.UI_Lang = @Lang and A.PContentID = @PContentID
			
		END
					
	END

END