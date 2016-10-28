package org.colomoto.logicalmodel.avatar;

import java.io.File;
import junit.framework.TestCase;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.avatar.AvatarImport;
import org.colomoto.logicalmodel.io.avatar.AvatarUtils;
import org.colomoto.logicalmodel.tool.inferinteraction.InteractionSearcher;
import org.colomoto.mddlib.PathSearcher;
import org.junit.Test;

public class AvatarPathSearch extends TestCase {

	@Test
	public void testAvatarPathSearch() {
		String fpath = "C:\\Users\\Rui\\Documents\\00 PosDoc\\Avatar Material\\table-models\\";
		String filename = fpath + "mmc-cycD1.avatar";
		try {
			AvatarImport avatar = new AvatarImport(new File(filename));
			LogicalModel model = avatar.getModel();
			
			/** Path searcher: working OK **/
			PathSearcher paths = new PathSearcher(model.getMDDManager());
			int[] path = paths.setNode(25);
			for(int leaf : paths) System.out.println("Path="+AvatarUtils.toString(path));

			/** Path searcher: working OK **/
			int[] testchildren = model.getMDDManager().getChildren(58);
			System.out.println("Path="+AvatarUtils.toString(testchildren));

			/** Interaction searcher: NOT working OK **/
			InteractionSearcher isearch = new InteractionSearcher(model);
			//isearch.run(); //problem of implementation of interaction searcher

		} catch (Exception e) {
			e.printStackTrace();
			System.out.println(e);
			fail(e.getMessage());
		} 
	}
}
