
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(ModelModifierService.class)

public class MostPermissive extends BaseService implements ModelModifierService {

}
